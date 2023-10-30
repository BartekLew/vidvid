use fdmux::*;
use std::io::{Error, Write};
use std::str;

trait Matchable {
    type Out;
    fn scan(inp: &str) -> Option<(&str, Self::Out)>;
}

impl Matchable for u64 {
    type Out=u64;

    fn scan(inp: &str) -> Option<(&str, u64)> {
        let it = inp.chars();
        let mut ans : u64 = 0;
        let mut i = 0;
        for c in it {
            match c.to_digit(10) {
                Some(x) => {
                    ans = ans*10 + x as u64;
                    i += 1;
                },
                None => break
            }
        }

        if i == 0 {
            None
        } else {
            Some((&inp[i..], ans))
        }
    }
}

struct Matcher<'a,T> {
    tail: &'a str,
    val: Option<T>
}

impl<'a,T> Matcher <'a,T> {
    fn then<F, R> (self, f: F) -> Matcher<'a, R>
            where F: Fn(&'a str, T) -> Matcher<'a, R> {
        match self.val {
            Some(v) => f(self.tail, v),
            None => Matcher { tail: self.tail, val: None }
        }
    }

    fn const_str(self, refs: &'a str) -> Self {
        self.then(|tail, val| {
            let mut it = tail.chars();
            for refc in refs.chars() {
                match it.next() {
                    Some(c) => match c == refc {
                                    true => (),
                                    false => return Matcher{ tail, val: None }
                            },
                    None => return Matcher { tail, val: None }
                }
            }

            Matcher { tail: it.as_str(), val: Some(val) }
        })
    }

    fn get<R: Matchable> (self) -> Matcher<'a,<R as Matchable>::Out> {
        self.then(|tail, _| {
            match R::scan(tail) {
                Some((t2, val)) => Matcher { tail: t2, val: Some(val) },
                None => Matcher { tail, val: None }
            }
        })
    }

    fn merge<R, F> (self, f: F) -> Matcher<'a, <R as Matchable>::Out>
            where R: Matchable,
                  F: Fn(T, <R as Matchable>::Out) -> <R as Matchable>::Out {
        self.then(|tail, v1| {
            match R::scan(tail) {
                Some((t2, v2)) => Matcher { tail: t2, val: Some(f(v1, v2)) },
                None => Matcher { tail, val: None }
            }
        })
    }
}

fn matcher<'a>(src: &'a str) -> Matcher<'a, ()> {
    Matcher { tail: src, val: Some(()) }
}


struct TimeCtl {
    time: u64
}

impl Write for TimeCtl {
    fn write(&mut self, buff: &[u8]) -> Result<usize, Error> {
        match str::from_utf8(buff) {
            Ok(s) => 
                match matcher(s).const_str("A: ")
                                .get::<u64>()
                                .const_str(".")
                                .merge::<u64, _>(|units, frac| units*10 + frac)
                                .val {
                    Some(x) => { self.time = x },
                    None => {}
                },
            Err(_) => {}
        }

        Ok(buff.len())
    }

    fn flush(&mut self) -> Result<(), Error> {
        Ok(())
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

fn main() {
    let cmdline: Vec<String> = std::env::args().skip(1).collect();
    match cmdline.is_empty() {
        true => {
            println!("usage: vidvid filename");
        },
        false => {
            let evreq = EvTraceRequest::new("MPlayer");
            let opipe = Pipe::new().unwrap();
            Process::new(vec!["mplayer", cmdline[0].as_str()])
                    .with_out(opipe.inp)
                    .spawn()
                    .unwrap()
                    .wait();
            let _evpipe = evreq.evpipe();
        }
    }
}
