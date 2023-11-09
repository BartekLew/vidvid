use pipes::*;
use std::io::{Error, Write};
use std::str;
use std::io::stdout;
use std::collections::HashMap;
use either::Either::Left as Left;
use either::Either::Right as Right;

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

/*
impl DoCtrlD for TimeCtl {
    fn ctrl_d(&mut self) -> bool {
        false
    }
}

struct EvTraceRequest {
    out : NamedReadPipe
}

impl EvTraceRequest {
    fn new(win_prefix: &str) -> Self {
        NamedWritePipe::new("/tmp/dwm.cmd".to_owned())
                       .expect("Can't open DWM command pipe")
                       .write(format!("k{}\n", win_prefix).as_bytes())
                       .unwrap();
        let out = NamedReadPipe::new("/tmp/dwm.out".to_owned())
                               .unwrap();

        EvTraceRequest { out }
    }

    fn evpipe(mut self) -> NamedReadPipe {
        NamedReadPipe::new(self.out.read_string().unwrap())
                     .unwrap()
    }
}
*/

fn main() {
    let cmdline: Vec<String> = std::env::args().skip(1).collect();
    match cmdline.is_empty() {
        true => {
            println!("usage: vidvid filename");
        },
        false => {
            //let evreq = EvTraceRequest::new("MPlayer");
            //let _evpipe = evreq.evpipe();

            let src = cmdline[0].as_str();
            let mut tctl = TimeCtl::new(src);
            match Call::new(vec!["mplayer", src])
                    .with_in(Pipe::new().unwrap())
                    .with_out(Pipe::new().unwrap())
                    .spawn()
                    .unwrap()
                    .streams() {
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
}

