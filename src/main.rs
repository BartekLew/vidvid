use pipes::*;
use std::io::{Error, Write};
use std::str;
use std::fmt;
use std::io::stdout;
use rand::Rng;
use std::collections::HashMap;
use either::Either::Left as Left;
use either::Either::Right as Right;
use std::{thread, time};
use std::fs;
use serde_derive::Serialize;
use serde_derive::Deserialize;
use std::rc::Rc;

mod matcher;
use matcher::*;

type TimePos = u64;
type Filename = Rc<String>;

#[derive(Serialize, Deserialize, Clone, Debug)]
struct OnFile<T> {
    file: Filename,
    val: T
}

impl OnFile<()> {
    fn new(file: Filename) -> Self {
        OnFile{ file, val: () }
    }
}

impl<T> OnFile<T> {
    fn make_sibling<V>(&self, val: V) -> OnFile<V> {
        OnFile { file: Rc::clone(&self.file), val }
    }

    fn on_same_file<V>(&self, other: &OnFile<V>) -> bool {
        self.file == other.file
    }

    fn combine<V,R,F>(self, other: OnFile<V>, transf: F) -> Result<OnFile<R>,String>
            where F: Fn(T, V) -> Result<R, String> {
        if self.file == other.file {
            match transf(self.val, other.val) {
                Ok(ans) => Ok(OnFile { file: self.file, val: ans } ),
                Err(e) => Err(e)
            }
        } else {
            Err(format!("Can't combine objects from differing files: ({}/{})", self.file, other.file))
        }
    }
}

enum FocusRequest {
    Repl, None
}

struct TimeCtl {
    time: TimePos,
}

impl TimeCtl {
    fn new() -> Self {
        TimeCtl{ time: 0 }
    }

    fn read_mplayer(&mut self, inp: &str) -> FocusRequest {
        let mut ans = FocusRequest::None;
        for ev in matcher(inp)
                   .many(|m| m.first_of(|m|
                               m.after("A:")
                                .skip_many(|m2| m2.white())
                                .value::<u64>()
                                .const_str(".")
                                .merge::<u64, TimePos, _>(|units, frac| units*10 + frac),
                           |m| m.after("===  PAUSE  ==="))) {

            match ev {
                Left(t) => {
                    self.time = t;
                },
                Right(()) => {
                    ans = FocusRequest::Repl;
                }
            }
        }

        ans
    }
}

#[derive(PartialEq)]
enum PromptResult {
    Continue,
    MoreRepl,
    Command(String),
    Error(String),
    Quit
}

impl PromptResult {
    fn cmd(&self) -> Option<String> {
        match self {
            Self::Continue => Some("pause\n".to_owned()),
            Self::MoreRepl => None,
            Self::Quit => None,
            Self::Command(s) => Some(format!("{}\n", s)),
            Self::Error(e) => panic!("no command to handle error: {}", e)
        }
    }
}

#[derive(Debug)]
enum TimeStamp {
    Relative(i64),
    Absolute(u64),
    Variable(String)
}

impl Matchable for TimeStamp {
    type Out = TimeStamp;

    fn scan(str: &str) -> Option<(&str, TimeStamp)> {
        matcher(str)
            .any(|m| m.white())
            .through(|m| m.option(&[CharClass::NonWhite]))
            .to_tupple()
            .and_then(|(tail, s)|
                matcher(s).or(|m| m.option(&[CharClass::Custom(&['+', '-'])])
                                   .merge::<u64, TimeStamp, _>(
                                       |s, v| TimeStamp::Relative (
                                                match s {
                                                    '+' => v as i64,
                                                    '-' => -(v as i64),
                                                    _ => panic!("unexpected value")
                                       })),
                              |m| m.value::<u64>()
                                   .map(|n| TimeStamp::Absolute(n)))
                          .result()
                          .map(|ans| match ans {
                                        Left(x) => (tail, x),
                                        Right(x) => (tail, x)
                                    })
                          .or(Some((tail, TimeStamp::Variable(s.to_owned())))))
    }
}

impl TimeStamp {
    fn evaluate(&self, current: TimePos, db: &TimeDb) -> Result<TimeVar,String> {
        match self {
            Self::Variable(name) =>
                match db.vars.get(name) {
                    Some(var) => Ok(var.clone()),
                    None => Err(format!("{} not a variable", name))
                },
            Self::Absolute(p) => Ok(TimeVar::Pos(db.scope.make_sibling(*p))),
            Self::Relative(off) => Ok(TimeVar::Pos(db.scope.make_sibling((
                                        current as i64 + off) as u64)))
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct Clip {
    start: TimePos,
    end: TimePos,
    stored: Option<Filename>
}

impl Clip {
    fn new(start: TimePos, end: TimePos) -> Self {
        Clip { start, end, stored: None }
    }
}

impl OnFile<Clip> {
    fn store(&mut self, filename: String) -> Result<(), String>{
        Call::new(vec!["ffmpeg", "-y",
                       "-ss", time_string(self.val.start).as_str(),
                       "-t", time_string(self.val.end-self.val.start).as_str(),
                       "-i", self.file.as_str(),
                       filename.as_str()])
            .spawn()
            .and_then(|p| p.wait())
            .and_then(|retcode|
                match retcode{
                    0 => {
                        self.val.stored = Some(Rc::new(filename));
                        Ok(())
                    },
                    _ => Err(format!("ffmpeg returned {}", retcode))
                })
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
enum TimeVar {
    Pos(OnFile<TimePos>),
    Range(OnFile<Clip>)
}

impl TimeVar {
    fn combine(self, other: TimeVar) -> Result<TimeVar, String> {
        match self {
            Self::Pos(a) => match other {
                Self::Pos(b) => a.combine(b, |s,e| Ok(Clip::new(s, e)))
                                 .map(|x| TimeVar::Range(x)),
                _ => Err(format!("Expected position signature, {} found", other))
            },
            _ => Err(format!("Expected position signature, {} found", self))
        }
    }
}

impl fmt::Display for TimeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pos(OnFile { file, val }) => write!(f, "{}.{} <{}>", val/10, val%10, file),
            Self::Range(OnFile { file, val: Clip {start, end, stored: None} }) =>
                write!(f, "{} -> {} <{}>", start/10, end/10, file),
            Self::Range(OnFile { file, val: Clip {start,end, stored: Some(name)} }) =>
                write!(f, "{} -> {} <{} -> {}>", start/10, end/10, file, name)
        }
    }
}

trait Command {
    fn name(&self) -> &'static str;
    fn run(&self, m: Matcher<()>, at_pos: TimePos, db: &mut TimeDb) -> PromptResult;

    fn push(self, tgt: &mut HashMap<&'static str, Box<dyn Command>>)
            where Self:Sized + 'static {
        tgt.insert(self.name(), Box::new(self));
    }
}

macro_rules! impl_command {
    ($struct_name:ident, $cmd: expr, $matcher:ident,
     $val:ident, $db: ident => $implementation:block) => {
        struct $struct_name {}

        impl Command for $struct_name {
            fn name(&self) -> &'static str {
                $cmd
            }

            fn run(&self, $matcher: Matcher<()>, $val: TimePos, $db: &mut TimeDb) -> PromptResult {
                $implementation
            }
        }
    };
}

impl_command!(Add, "add", m, val, db => {
    match m.any(|m| m.white()).word().result() {
        Some(arg) => { db.vars.insert(arg.to_owned(), TimeVar::Pos(db.scope.make_sibling(val))); },
        None => { db.vars.insert(format!("#{}", db.vars.len()),
                                 TimeVar::Pos(db.scope.make_sibling(val))); }
    }
    PromptResult::MoreRepl
});

fn provided<A,B,R,F>(a: Result<A,String>, b: Result<B,String>, act: F) -> Result<R,String>
        where F : Fn(A, B) -> Result<R,String> {
    a.and_then(|a| b.and_then (|b| act(a,b)))
}

impl_command!(Take, "take", m, current_time, db => {
    match m.any(|m| m.white())
           .through(|m| m.option(&[CharClass::NonWhite]))
           .to_tupple() {
        Some((tail, name)) => 
           match matcher(tail)
                        .many(|m| m.value::<TimeStamp>())
                        .take(2)
                        .map(|ts| ts.evaluate(current_time, &db))
                        .reduce(|a, b| provided(a,b, |a, b| a.combine(b))) {
                Some(Ok(range)) => {
                    db.vars.insert(name.to_string(), range);
                    PromptResult::MoreRepl
                }
                Some(Err(e)) => {
                    PromptResult::Error(e)
                }
                None => PromptResult::Error("Expected timespec at line end".to_string()),
            },
        None => PromptResult::Error("Expected name".to_string())
    }});

impl_command!(Dump, "dump", _m, _val, db => {
    println!("{}", serde_json::to_string(&db.vars).unwrap());
    PromptResult::MoreRepl
});

impl_command!(Seek, "seek", m, current, db => {
    match m.value::<TimeStamp>()
           .result() {
        Some(pos) => {
            match pos.evaluate(current, &db) {
                Ok(range) => {
                    return db.seek(range);
                }
                Err(e) => eprintln!("Error: {}", e)
            }
        },
        None => eprintln!("Missing argument")
    };

    PromptResult::Continue
});

fn time_string(t: TimePos) -> String {
    format!("{}.{}", t/10, t%10)
}

impl_command!(Save, "save", m, _val, db => {
    match m.value::<TimeStamp>()
            .result() {
        Some(TimeStamp::Variable(name)) => {
            match db.vars.get_mut(&name) {
                Some(TimeVar::Range(r)) => match r.store(format!("{}.avi", name)) {
                                             Ok(_) => PromptResult::MoreRepl,
                                             Err(e) => PromptResult::Error(e)
                                           },
                _ => PromptResult::Error(format!("Range variable not found: {}", name))
            }
        },
        _ => PromptResult::Error("Expected variable.".to_string())
    }
});

struct TimeDb {
    scope: OnFile<()>,
    vars: HashMap<String, TimeVar>,
    trap: Option<TimePos>
}

impl TimeDb {
    pub fn seek(&mut self, time: TimeVar) -> PromptResult {
        match time {
            TimeVar::Pos(ref p) => self._seek(p),
            TimeVar::Range(OnFile{file, val: Clip {start, end, ..}}) => {
                self.trap = Some(end);
                self._seek(&OnFile {file, val: start })
            }
        }
    }

    fn _seek(&mut self, pos: &OnFile<TimePos>) -> PromptResult {
        if !self.scope.on_same_file(pos) {
            self.scope = pos.make_sibling(());
            return PromptResult::Command(format!("loadfile \"{}\"\nseek {} 2",
                                                 self.scope.file, pos.val/10))
        } else {
           return PromptResult::Command(format!("seek {} 2", pos.val/10))
        }
    }
}

struct Repl {
    handlers: HashMap<&'static str, Box<dyn Command>>,
    db: TimeDb,
    inp: ReadPipe
}

const TIMEDB_STORE: &str = ".vidvid";
impl Repl {
    fn new(source: &str) -> Self {
        Repl {
            handlers: {
                let mut x = HashMap::new();
                Add{}.push(&mut x);
                Seek{}.push(&mut x);
                Take{}.push(&mut x);
                Dump{}.push(&mut x);
                Save{}.push(&mut x);
                x
            },
            db: TimeDb {
                scope: OnFile::new(Rc::new(source.to_string())),
                vars: match fs::read(TIMEDB_STORE) {
                    Ok(src) => serde_json::from_str(&String::from_utf8_lossy(&src[..]))
                                         .unwrap_or(HashMap::new()),
                    Err(_) => HashMap::new()
                },
                trap: None
            },
            inp: ReadPipe::stdin()
        }
    }

    fn time_prompt(&mut self, val: TimePos) -> PromptResult {
        self.dump();
        stdout().write(format!("{}> ",val).as_bytes()).unwrap();
        stdout().flush().unwrap();

        match self.inp.read_str() {
            Err(_) => PromptResult::Quit,
            Ok(s) => {
                match matcher(s.as_str()).any(|m| m.white()).word().to_tupple() {
                    Some((tail, cmd)) => match self.handlers.get(cmd) {
                        Some(handler) => handler.run(matcher(tail), val, &mut self.db),
                        None => PromptResult::Error(format!("Unknown command: {}", cmd))
                    },
                    None => PromptResult::Continue
                }
            }
        }
    }

    fn dump(&self) {
        for k in self.db.vars.keys() {
            println!("{}:{}", k, self.db.vars[k]);
        }
    }

    fn active_trap(&self) -> Option<TimePos> {
        self.db.trap
    }
}

impl Drop for Repl {
    fn drop(&mut self) {
        fs::write(TIMEDB_STORE, serde_json::to_string(&self.db.vars).unwrap())
            .unwrap();
    }
}

struct ReplWrapper {
    cmd: DwmCmd,
    tctl: TimeCtl,
    repl : Repl,
    mplayer_in: WritePipe,
    player_wid: u64,
    term_wid: u64
}

impl ReplWrapper {
    fn prompt(&mut self) {
        if let Err(e) = self.cmd.focus(self.term_wid) {
            eprintln!("warning: {}", e);
        }
        loop {
            match self.repl.time_prompt(self.tctl.time) {
                PromptResult::Quit => return,
                    PromptResult::Error(e) => println!("{}", e),
                    result => {
                        result.cmd().map(|c| self.mplayer_in.write(c.as_bytes()).unwrap());
                        if PromptResult::MoreRepl != result {
                            self.cmd.focus(self.player_wid).unwrap();
                            break;
                        }
                    },
            }
        }
    }

    fn read_mplayer(&mut self, s: String) {
        match self.tctl.read_mplayer(s.as_str()) {
            FocusRequest::Repl => self.prompt(),
            _ => {
                match self.repl.active_trap() {
                    Some(pos) => {
                        if pos <= self.tctl.time {
                            self.mplayer_in.write("\npause\n".as_bytes()).unwrap();
                        } 
                    }, None => {}
                }
            }
        }
    }
}

struct VidVid {
    cmd: DwmCmd,
    term_wid: u64,
    player_wid: Option<u64>,
    player_proc: Option<Process>,
    repl: Option<Repl>
}

impl VidVid {
    fn new() -> Self {
        let title = format!("vidvid-{:#x}", rand::thread_rng().gen::<u64>() % 0x10000);
        let mut cmd = DwmCmd::new().unwrap();
        VidVid { term_wid: cmd.get_my_win(title.as_str())
                              .unwrap(),
                 player_wid: None,
                 player_proc: None, 
                 repl: None,
                 cmd }
    }

    fn spawn_player(mut self, file: &str) -> Result<Self,String> {
        self.cmd.trap_keygrab("MPlayer")?;        
        self.player_proc = Some(
            Call::new(vec!["mplayer", "-slave", file])
                .with_in(Pipe::new().unwrap())
                .with_out(Pipe::new().unwrap())
                .spawn()?);
        self.player_wid = Some(self.cmd.recv_wid()?);
        self.repl = Some(Repl::new(file));
        Ok(self)
    }

    fn run(self, tctl: TimeCtl) {
        match self.player_proc.unwrap().streams() {
            (Some(mplayer_in), Some(mut out), _) => {
                let mut repl = ReplWrapper {
                    cmd: self.cmd,
                    tctl,
                    repl: self.repl.unwrap(),
                    mplayer_in,
                    player_wid: self.player_wid.unwrap(),
                    term_wid: self.term_wid
                };

                loop {
                    match out.read_str() {
                        Ok(s) => repl.read_mplayer(s),
                        Err(_) => break
                    }
                }
            },
            _ => {}
        }
    }
}

struct DwmCmd {
    inp: WritePipe,
    out: ReadPipe
}

impl DwmCmd {
    fn new() -> Result<Self, String> {
        Ok(DwmCmd { inp: WritePipe::open("/tmp/dwm.cmd")?,
                    out: ReadPipe::open("/tmp/dwm.out")? })
    }

    pub fn focus(&mut self, wid: u64) -> Result<(), String> {
        self.inp.write(format!("F{:x}\n", wid).as_bytes())
                .and_then(|_| self.inp.flush())
                .or(Err("Can't write focus request".to_owned()))
    }

    pub fn trap_keygrab(&mut self, title: &str) -> Result<(), String> {
        self.inp.write(format!("k{}\n", title).as_bytes())
            .map(|_|{}).or(Err("Can't write DwmCommand".to_string()))
    }

    pub fn recv_wid(&mut self) -> Result<u64, String> {
        self.out.read_str()
            .or(Err("Can't read DwmCmd".to_string()))
            .and_then(|s| matcher(s.as_str())
                           .after("-MPlayer-")
                           .value::<u64>()
                           .result()
                           .ok_or(format!("Invalid wid: {}", s)))
    }

    pub fn get_my_win(&mut self, title: &str) -> Result<u64, String> {
        set_term_title(title).expect("Can't write&flush output");
        
        // wait for change take place. DWM won't wait for it :(
        thread::sleep(time::Duration::from_millis(10));

        self.inp.write(format!("d{}\n", title).as_bytes())
            .or(Err("Can't write to DwmCmd".to_owned()))
            .and_then(|_| self.out.read_str()
                              .or(Err("Can't read from DwmCmd".to_owned()))
                              .and_then(|resp| matcher(resp.as_str())
                                                   .value::<u64>()
                                                   .result()
                                                   .ok_or(format!("Can't parse window ID: {}", resp))))
    }

}

fn set_term_title(title: &str) -> Result<(), Error> {
    println!("\x1b]0;{}\x07", title);
    Ok(())
}

fn main() {
    let cmdline: Vec<String> = std::env::args().skip(1).collect();
    match cmdline.is_empty() {
        true => {
            println!("usage: vidvid filename");
        },
        false => {
            let src = cmdline[0].as_str();
            VidVid::new()
                  .spawn_player(src).unwrap()
                  .run(TimeCtl::new());
        }
    }
}

