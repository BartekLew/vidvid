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

struct TimeCtl<'a> {
    time: TimePos,
    db: TimeDb<'a>
}

impl<'a> TimeCtl<'a> {
    fn new(src: &'a str) -> Self {
        TimeCtl{ time: 0, db: TimeDb::new(src) }
    }
}

impl<'a> Write for TimeCtl<'a> {
    fn write(&mut self, buff: &[u8]) -> Result<usize, Error> {
        match str::from_utf8(buff) {
            Ok(s) => 
                for ev in matcher(s)
                           .many(|m| m.first_of(|m|
                                       m.after("A:")
                                        .skip_many(|m2| m2.white())
                                        .value::<u64>()
                                        .const_str(".")
                                        .merge::<u64, TimePos, _>(|units, frac| units*10 + frac),
                                   |m| m.after("===  PAUSE  ==="))) {

                    match ev {
                        Left(t) => {
                            self.time = t
                        },
                        Right(()) => {
                            self.db.time_prompt(self.time)
                        }
                    }
                },
            Err(_) => {}
        }

        Ok(buff.len())
    }

    fn flush(&mut self) -> Result<(), Error> {
        Ok(())
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

    fn time_prompt(&mut self, val: TimePos) {
        self.dump();
        stdout().write(format!("{}> ",val).as_bytes()).unwrap();
        stdout().flush().unwrap();

        self.inp.read_str()
                .map(|s| {
            let mut it = matcher(s.as_str()).many(|ws| ws.until_word());
            match it.next() {
                Some(c) => match c {
                    "add" => match it.next() {
                        Some(arg) => { self.data.insert(arg.to_owned(), val); },
                        None => { self.data.insert(format!("#{}", self.data.len()), val); }
                    },
                    cmd => println!("Unknown command: {}", cmd)
                }, None => {}
            }
        }).unwrap_or_else(|e| println!("{}",e));
    }

    fn dump(&self) {
        for k in self.data.keys() {
            println!("{}:{}", k, self.data[k]);
        }
    }
}

struct VidVid {
    cmd: DwmCmd,
    _term_wid: u64,
    player_wid: Option<u64>,
    player_proc: Option<Process>
}

impl VidVid {
    fn new() -> Self {
        let title = format!("vidvid-{:#x}", rand::thread_rng().gen::<u64>() % 0x10000);
        let mut cmd = DwmCmd::new().unwrap();
        VidVid { _term_wid: cmd.get_my_win(title.as_str())
                              .unwrap(),
                 player_wid: None,
                 player_proc: None, 
                 cmd }
    }

    fn spawn_player(mut self, file: &str) -> Result<Self,String> {
        self.cmd.trap_keygrab("MPlayer")?;        
        self.player_proc = Some(
            Call::new(vec!["mplayer", file])
                .with_in(Pipe::new().unwrap())
                .with_out(Pipe::new().unwrap())
                .spawn()?);
        self.player_wid = Some(self.cmd.recv_wid()?);
        Ok(self)
    }

    fn run(self, mut tctl: TimeCtl) {
        match self.player_proc.unwrap().streams() {
            (_, Some(mut out), _) => {
                loop {
                    match out.read_str() {
                        Ok(s) => {
                            tctl.write(s.as_bytes()).unwrap();
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
                  .run(TimeCtl::new(src));
        }
    }
}

