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

mod matcher;
use matcher::*;

type TimePos = u64;

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
    fn as_pos(&self, base:TimePos, vars: &HashMap<String, TimeVar>) -> Result<TimePos, String> {
        match self {
            Self::Absolute(ts) => Ok(*ts),
            Self::Relative(off) => Ok((base as i64 + off) as u64),
            Self::Variable(name) => match vars.get(name) {
                                        Some(TimeVar::Pos(val)) => Ok(*val),
                                        Some(TimeVar::Range(val, _)) => {
                                            eprintln!("Warning, using range  {} as pos!", name);
                                            Ok(*val)
                                        } None => Err(format!("Variable {} does not exist.", name))
                                    }}
    }
}

#[derive(Serialize, Deserialize)]
enum TimeVar {
    Pos(TimePos),
    Range(TimePos,TimePos)
}

impl fmt::Display for TimeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pos(pos) => write!(f, "{}.{}", pos/10, pos%10),
            Self::Range(s,e) => write!(f, "{} -> {}", s/10, e/10)
        }
    }
}

trait Command {
    fn name(&self) -> &'static str;
    fn run(&self, m: Matcher<()>, at_pos: TimePos, db: &mut HashMap<String, TimeVar>) -> PromptResult;

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

            fn run(&self, $matcher: Matcher<()>, $val: TimePos, $db: &mut HashMap<String, TimeVar>) -> PromptResult {
                $implementation
            }
        }
    };
}

impl_command!(Add, "add", m, val, db => {
    match m.any(|m| m.white()).word().result() {
        Some(arg) => { db.insert(arg.to_owned(), TimeVar::Pos(val)); },
        None => { db.insert(format!("#{}", db.len()), TimeVar::Pos(val)); }
    }
    PromptResult::MoreRepl
});

impl_command!(Seek, "seek", m, _val, db => {
    match m.value::<TimeStamp>().result() {
        Some(TimeStamp::Absolute(ts)) =>
            PromptResult::Command(format!("seek {} 2", ts)),
        Some(TimeStamp::Relative(ts)) =>
            PromptResult::Command(format!("seek {}", ts)),
        Some(TimeStamp::Variable(valname)) =>
            match db.get(&valname) {
                Some(v) => PromptResult::Command(format!("seek {} 2", v)),
                None => PromptResult::Error(format!("Unknown variable: {}", valname))
        },
        None => PromptResult::Error("seek requires argument ([+/-] seconds)"
                                        .to_owned())
    }
});

impl_command!(Take, "take", m, val, db => {
    match m.any(|m| m.white())
           .through(|m| m.option(&[CharClass::NonWhite]))
           .to_tupple() {
        Some((tail, name)) => 
           match matcher(tail)
                        .value::<TimeStamp>()
                        .merge::<TimeStamp, (TimeStamp, TimeStamp),_>(|a, b| (a,b))
                        .result() {
                Some((start, end)) => {
                    start.as_pos(val, db)
                         .and_then(|s| end.as_pos(val,db)
                                          .and_then(|e| {
                                              println!("insert {} {}->{}", name, s, e);
                                              db.insert(name.to_string(), TimeVar::Range(s,e));
                                              Ok(PromptResult::MoreRepl)
                                          }))
                         .unwrap_or_else(|e| PromptResult::Error(e))
                },
                None => PromptResult::Error("Expected timespec at line end".to_string()),
            },
        None => PromptResult::Error("Expected name".to_string())
    }
});

impl_command!(Dump, "dump", _m, _val, db => {
    println!("{}", serde_json::to_string(&db).unwrap());
    PromptResult::MoreRepl
});

struct TimeDb<'a> {
    _source: &'a str,
    handlers: HashMap<&'static str, Box<dyn Command>>,
    data: HashMap<String, TimeVar>,
    inp: ReadPipe
}

const TIMEDB_STORE: &str = ".vidvid";
impl<'a> TimeDb<'a> {
    fn new(source: &'a str) -> Self {
        TimeDb {
            _source: source, 
            handlers: {
                let mut x = HashMap::new();
                Add{}.push(&mut x);
                Seek{}.push(&mut x);
                Take{}.push(&mut x);
                Dump{}.push(&mut x);
                x
            },
            data: match fs::read(TIMEDB_STORE) {
                Ok(src) => serde_json::from_str(&String::from_utf8_lossy(&src[..]))
                                     .unwrap_or(HashMap::new()),
                Err(_) => HashMap::new()
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
                        Some(handler) => handler.run(matcher(tail), val, &mut self.data),
                        None => PromptResult::Error(format!("Unknown command: {}", cmd))
                    },
                    None => PromptResult::Continue
                }
            }
        }
    }

    fn dump(&self) {
        for k in self.data.keys() {
            println!("{}:{}", k, self.data[k]);
        }
    }
}

impl<'a> Drop for TimeDb<'a> {
    fn drop(&mut self) {
        fs::write(TIMEDB_STORE, serde_json::to_string(&self.data).unwrap())
            .unwrap();
    }
}

struct VidVid<'a> {
    cmd: DwmCmd,
    term_wid: u64,
    player_wid: Option<u64>,
    player_proc: Option<Process>,
    db: Option<TimeDb<'a>>
}

impl<'a> VidVid<'a> {
    fn new() -> Self {
        let title = format!("vidvid-{:#x}", rand::thread_rng().gen::<u64>() % 0x10000);
        let mut cmd = DwmCmd::new().unwrap();
        VidVid { term_wid: cmd.get_my_win(title.as_str())
                              .unwrap(),
                 player_wid: None,
                 player_proc: None, 
                 db: None,
                 cmd }
    }

    fn spawn_player(mut self, file: &'a str) -> Result<Self,String> {
        self.cmd.trap_keygrab("MPlayer")?;        
        self.player_proc = Some(
            Call::new(vec!["mplayer", "-slave", file])
                .with_in(Pipe::new().unwrap())
                .with_out(Pipe::new().unwrap())
                .spawn()?);
        self.player_wid = Some(self.cmd.recv_wid()?);
        self.db = Some(TimeDb::new(file));
        Ok(self)
    }

    fn run(mut self, mut tctl: TimeCtl) {
        match self.player_proc.unwrap().streams() {
            (Some(mut inp), Some(mut out), _) => {
                loop {
                    match out.read_str() {
                        Ok(s) => {
                            match tctl.read_mplayer(s.as_str()) {
                                FocusRequest::Repl => {
                                    if let Err(e) = self.cmd.focus(self.term_wid) {
                                        eprintln!("warning: {}", e);
                                    }
                                    loop {
                                        match self.db.as_mut()
                                                .unwrap()
                                                .time_prompt(tctl.time) {
                                            PromptResult::Quit => return,
                                            PromptResult::Error(e) => println!("{}", e),
                                            result => {
                                                result.cmd().map(|c| inp.write(c.as_bytes()).unwrap());
                                                if PromptResult::MoreRepl != result {
                                                    self.cmd.focus(self.player_wid.unwrap()).unwrap();
                                                    break;
                                                }
                                            },
                                        }
                                    }
                                },
                                _ => {}
                            }
                        },
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

