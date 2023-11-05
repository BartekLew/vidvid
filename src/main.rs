use pipes::*;
use std::io::{Error, Write};
use std::str;
use either::Either::Left as Left;
use either::Either::Right as Right;

mod matcher;
use matcher::*;

struct TimeCtl {
    time: u64
}

impl TimeCtl {
    fn new() -> Self {
        TimeCtl{ time: 0 }
    }
}

impl Write for TimeCtl {
    fn write(&mut self, buff: &[u8]) -> Result<usize, Error> {
        match str::from_utf8(buff) {
            Ok(s) => 
                for ev in matcher(s)
                           .many(|m| m.first_of(|m|
                                       m.after("A:")
                                        .skip_many(|m2| m2.white())
                                        .value::<u64>()
                                        .const_str(".")
                                        .merge::<u64, u64, _>(|units, frac| units*10 + frac),
                                   |m| m.after("===  PAUSE  ==="))) {

                    match ev {
                        Left(t) => {
                            self.time = t
                        },
                        Right(()) => {
                            println!(">> {}", self.time);
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

            let mut tctl = TimeCtl::new();
            match Call::new(vec!["mplayer", cmdline[0].as_str()])
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

