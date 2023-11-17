use pipes::*;
use std::io::{Error, Write};
use std::str;
use std::io::stdout;
use rand::Rng;
use std::collections::HashMap;
use either::Either::Left as Left;
use either::Either::Right as Right;
use std::{thread, time};

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

struct TimeDb<'a> {
    _source: &'a str,
    data: HashMap<String, TimePos>,
    inp: ReadPipe
}

impl<'a> TimeDb<'a> {
    fn new(source: &'a str) -> Self {
        TimeDb { _source: source, data: HashMap::new(), inp: ReadPipe::stdin() }
    }

    fn time_prompt(&mut self, val: TimePos) -> Result<(), String> {
        self.dump();
        stdout().write(format!("{}> ",val).as_bytes()).unwrap();
        stdout().flush().unwrap();

        self.inp.read_str()
                .or_else(|e| Err(format!("Read error: {}", e)))
                .and_then(|s| {
            let mut it = matcher(s.as_str()).many(|ws| ws.until_word());
            match it.next() {
                Some(c) => match c {
                    "add" => {
                        match it.next() {
                            Some(arg) => { self.data.insert(arg.to_owned(), val); },
                            None => { self.data.insert(format!("#{}", self.data.len()), val); }
                        }
                        Ok(())
                    },
                    cmd => Err(format!("Unknown command: {}", cmd))
                }, None => Ok(())
            }
        })
    }

    fn dump(&self) {
        for k in self.data.keys() {
            println!("{}:{}", k, self.data[k]);
        }
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
                                            Ok(()) => {
                                                self.cmd.focus(self.player_wid.unwrap()).unwrap();
                                                inp.write("pause\n".as_bytes()).unwrap();
                                                break;
                                            },
                                            Err(e) => println!("{}", e)
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

