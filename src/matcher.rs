use std::str;
use either::Either;
use either::Either::Left as Left;
use either::Either::Right as Right;

pub trait HasNone {
    fn none() -> Self;
}

impl HasNone for () {
    fn none() -> Self { () }
}

pub trait Matchable {
    type Out;
    fn scan(inp: &str) -> Option<(&str, Self::Out)>;
}

impl Matchable for u64 {
    type Out=u64;

    fn scan(inp: &str) -> Option<(&str, u64)> {
        fn scan_with_base(inp: &str, base: u32) -> Option<(&str, u64)> {
            let mut ans = 0;
            let mut i = 0;
            for c in matcher(inp)
                            .many(|m| m.option(&[CharClass::Digit(base)])) {
                match c.to_digit(base) {
                    Some(x) => {
                        ans = ans*(base as u64) + x as u64;
                        i += 1;
                    },
                    None => panic!("Internal error")
                }
            }

            if i > 0 {
                Some((&inp[i..], ans))
            } else {
                None
            }
        }

        if inp.len() > 2 && &inp[0..2] == "0x" {
            scan_with_base(&inp[2..], 16)            
        } else {
            scan_with_base(&inp, 10)            
        }
    }
}

pub struct MatcherLoop<'a, R, F>
where
    F: Fn(Matcher<'a, ()>) -> Matcher<'a, R>,
{
    tail: Option<&'a str>,
    f: F,
}

impl<'a, R, F> Iterator for MatcherLoop<'a, R, F>
where
    F: Fn(Matcher<'a, ()>) -> Matcher<'a, R>,
{
    type Item = R;

    fn next(&mut self) -> Option<Self::Item> {
        let tail = self.tail.take();
        let m = (self.f)(matcher(tail?));
        self.tail = Some(m.tail);
        m.val
    }
}

impl<'a, R, F> MatcherLoop<'a, R, F> 
    where
        F: Fn(Matcher<'a, ()>) -> Matcher<'a, R> {
    pub fn then(self) -> Matcher<'a, ()> {
        matcher(self.tail.unwrap())
    }
}

pub enum CharClass<'a> {
    Letter,
    Digit(u32),
    White,
    NonWhite,
    Custom(&'a[char])
}

impl<'a> CharClass<'a> {
    fn is_match(&self, c: char) -> bool {
        match self {
            Self::Letter => c.is_alphabetic(),
            Self::Digit(base) => c.is_digit(*base),
            Self::White => c.is_whitespace(),
            Self::NonWhite => !c.is_whitespace(),
            Self::Custom(cs) => cs.contains(&c)
        }
    }
}

pub struct Matcher<'a,T> {
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

    pub fn after(mut self, refs: &'a str) -> Self {
        let mut finder = matcher(self.tail);
        while finder.tail.len() > refs.len() {
            finder = finder.const_str(refs);
            if finder.val.is_some() {
                self.tail = finder.tail;
                return self;
            }
            finder.tail = &finder.tail[1..];
            finder.val = Some(());
        }

        self.val = None;
        return self;
    }

    pub fn until_word(self) -> Matcher<'a, &'a str> {
        let mut cur = self.tail;
        let mut ans;
        while cur.len() > 0 {
            ans = matcher(cur).word();
            if ans.val.is_some() {
                return ans;
            }
            cur = &cur[1..];
        }

        Matcher { tail: self.tail, val: None }
    }

    #[cfg(test)]
    pub fn until<R: Matchable>(self) -> Matcher<'a, <R as Matchable>::Out> {
        let mut cur = self.tail;
        let mut ans;
        while cur.len() > 0 {
            ans = matcher(cur).value::<R>();
            if ans.val.is_some() {
                return ans;
            }
            cur = &cur[1..];
        }

        Matcher { tail: self.tail, val: None }
    }

    pub fn many<R,F>(self, f:F) -> MatcherLoop<'a,R,F>
            where F: Fn(Matcher<'a, ()>) -> Matcher<'a, R> {
        MatcherLoop { tail: Some(self.tail), f }
    }

    pub fn or<L,R, F, G>(self, f:F, g:G) -> Matcher<'a, Either<L, R>>
                where F: Fn(Matcher<'a, ()>) -> Matcher<'a, L>,
                      G: Fn(Matcher<'a, ()>) -> Matcher<'a, R> {
        let l = f(matcher(self.tail));
        match l.val {
            Some(v) => Matcher { tail: l.tail, val: Some(Left(v)) },
            None => {
                let r = g(matcher(self.tail));
                match r.val {
                    Some(v) => Matcher {tail: r.tail, val: Some(Right(v))},
                    None => Matcher { tail: self.tail, val: None }
                }
            }
        }
    }

    pub fn first_of<L,R, F, G>(self, f:F, g:G) -> Matcher<'a, Either<L, R>>
                where F: Fn(Matcher<'a, ()>) -> Matcher<'a, L>,
                      G: Fn(Matcher<'a, ()>) -> Matcher<'a, R> {
        let l = f(matcher(self.tail));
        let r = g(matcher(self.tail));

        if l.val.is_some() && (r.val.is_none() || l.tail.len() > r.tail.len()) {
            Matcher { tail: l.tail, val: Some(Left(l.val.unwrap())) }
        } else if r.val.is_some() {
            Matcher {tail: r.tail, val: Some(Right(r.val.unwrap()))}
        } else {
            Matcher { tail: self.tail, val: None }
        }
    }

    pub fn skip_many<R,F>(mut self, f:F) -> Self 
            where F: for <'b> Fn(Matcher<'b, ()>) -> Matcher<'b, R> {
        let mut it = MatcherLoop { tail: Some(self.tail), f };
        while it.next().is_some() {
            self.tail = it.tail.unwrap();
        }
        self
    }

    pub fn any<R,F>(mut self, f:F) -> Matcher<'a, R>
            where F: Fn(Matcher<'a, ()>) -> Matcher<'a, R>,
                  R: HasNone {
        let mut it = MatcherLoop { tail: Some(self.tail), f };
        while it.next().is_some() {
            self.tail = it.tail.unwrap();
        }
        
        Matcher { tail: self.tail, val: Some(R::none()) }
    }

    pub fn through<R,F>(mut self, f:F) -> Matcher<'a, &'a str>
            where F: Fn(Matcher<'a, ()>) -> Matcher<'a, R> {
        let start = self.tail;
        let mut it = MatcherLoop { tail: Some(self.tail), f };
        while it.next().is_some() {
            self.tail = it.tail.unwrap();
        }
        
        let len =  start.len() - self.tail.len();
        if len > 0 {
            Matcher { tail: self.tail, val: Some(&start[0..len]) }
        } else {
            Matcher { tail: start, val: None }
        }
    }

    pub fn map<R,F>(self, f:F) -> Matcher<'a, R>
            where F: Fn(T) -> R {
        self.then(|tail, val| Matcher { tail, val: Some(f(val)) })
    }

    fn word(self) -> Matcher<'a, &'a str> {
        self.then(|tail, _|
            matcher(tail)
                .through(|m| m.option(&[CharClass::Letter])))
    }

    pub fn white(self) -> Self {
        self.then(|tail, val| 
            match matcher(tail).option(&[CharClass::White]).to_tupple() {
                Some((tail,_)) => Matcher { tail, val: Some(val) },
                None => Matcher { tail, val: None }
            })
    }

    pub fn option(self, options: &[CharClass]) -> Matcher<'a, char> {
        self.then(|str, _| {
            if let Some(c) = str.chars().nth(0) {
               if options.iter().any(|o| o.is_match(c)) {
                   return Matcher { tail: &str[1..], val: Some(c) };
               }
            }

            Matcher { tail: str, val: None }
        })
    }

    pub fn const_str(self, refs: &'a str) -> Self {
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

    pub fn value<R: Matchable> (self) -> Matcher<'a,<R as Matchable>::Out> {
        self.then(|tail, _| {
            match R::scan(tail) {
                Some((t2, val)) => Matcher { tail: t2, val: Some(val) },
                None => Matcher { tail, val: None }
            }
        })
    }

    pub fn merge<R, V, F> (self, f: F) -> Matcher<'a, V>
            where R: Matchable,
                  F: Fn(T, <R as Matchable>::Out) -> V {
        self.then(|tail, v1| {
            match R::scan(tail) {
                Some((t2, v2)) => Matcher { tail: t2, val: Some(f(v1, v2)) },
                None => Matcher { tail, val: None }
            }
        })
    }

    pub fn result(self) -> Option<T> {
        self.val
    }

    pub fn to_tupple(self) -> Option<(&'a str, T)> {
        self.val.map(|v| (self.tail, v))
    }

    #[cfg(test)]
    fn result_ref(&self) -> &Option<T> {
        &self.val
    }
}

pub fn matcher<'a>(src: &'a str) -> Matcher<'a, ()> {
    Matcher { tail: src, val: Some(()) }
}

#[cfg(test)]
mod test {
    use crate::matcher::*;

    #[test]
    fn matcher_matches_number () {
        let m = matcher("foo: 42 bar")
                    .const_str("foo: ")
                    .value::<u64>()
                    .result();
        assert_eq!(m, Some(42));
    }

    #[test]
    fn matcher_doesnt_match_other () {
        let m = matcher("foo bar baz")
                    .const_str("bar")
                    .result();
        assert_eq!(m, None);
    }

    #[test]
    fn matcher_stops_matching_on_fail () {
        let m = matcher("42 foo bar")
                    .const_str("bar")
                    .value::<u64>()
                    .result();
        assert_eq!(m, None);
    }

    #[test]
    fn matcher_merges_values () {
        let m = matcher("= 3 + 4")
                    .const_str("= ")
                    .value::<u64>()
                    .const_str(" + ")
                    .merge::<u64, u64, _>(|a, b| a + b)
                    .result();
        assert_eq!(m, Some(7));
    }

    #[test]
    fn matcher_stops_on_merge_wrong_merge() {
        let m = matcher("4 / k10")
                    .value::<u64>();
        assert_eq!(m.result_ref(), &Some(4));

        let m2 = m.const_str(" / ");
        assert_eq!(m2.result_ref(), &Some(4));

        let m3 = m2.merge::<u64, u64,_>(|a, b| a / b)
                            .result();

        assert_eq!(m3, None);
    }

    #[test]
    fn matcher_matches_after () {
        let m = matcher("foo bar baz 43 11")
                    .after(" baz ")
                    .value::<u64>()
                    .result();

        assert_eq!(m, Some(43));
    }

    #[test]
    fn matcher_matches_words () {
        let m = matcher("fas; ca")
                    .word()
                    .result();
        assert_eq!(m, Some("fas"));
    }

    #[test]
    fn matcher_searches_word() {
        let m = matcher(">> foo = 32 baz")
                        .until_word()
                        .const_str(" = ")
                        .merge::<u64, (&str, u64), _>(|name, val| (name,val))
                        .result();
        assert_eq!(m, Some(("foo", 32)));
    }

    #[test]
    fn matcher_matches_many_numbers() {
        let mut it = matcher("43 31 533")
                       .many(|m1| m1.until::<u64>());

        assert_eq!(it.next(), Some(43));
        assert_eq!(it.next(), Some(31));
        assert_eq!(it.next(), Some(533));
        assert_eq!(it.next(), None);

    }

    #[test]
    fn matcher_supports_hexdecimal_values() {
        let ans = matcher(" - 0xaa")
                        .after(" - ")
                        .value::<u64>()
                        .result();

        assert_eq!(ans, Some(0xaa));
    }

    #[test]
    fn matcher_matches_many () {
        let mut it = matcher("foo=1 bar=2 zza=3")
                        .many(|m1|
                                m1.until_word()
                                .const_str("=")
                                .merge::<u64,(&str, u64),_>(|name, val| (name,val)));

        assert_eq!(it.next(), Some(("foo",1)));
        assert_eq!(it.next(), Some(("bar",2)));
        assert_eq!(it.next(), Some(("zza",3)));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn matcher_matches_any_amount_of_whitespace() {
        let mut it = matcher("my wonderful data:        42 11 15")
                        .after("data:")
                        .many(|m| m.skip_many(|m| m.white())
                                   .value::<u64>());

        assert_eq!(it.next(), Some(42));
        assert_eq!(it.next(), Some(11));
        assert_eq!(it.next(), Some(15));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn matcher_supports_alternative() {
        let mut it = matcher("foo 32 bar baz 42")
                        .many(|m| m.any(|m| m.white())
                                    .or(|m| m.word(),
                                        |m| m.value::<u64>()));

        assert_eq!(it.next(), Some(Left("foo")));
        assert_eq!(it.next(), Some(Right(32)));
        assert_eq!(it.next(), Some(Left("bar")));
        assert_eq!(it.next(), Some(Left("baz")));
        assert_eq!(it.next(), Some(Right(42)));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn matcher_supports_alternative_with_precedence() {
        let mut it = matcher("zbad *44* ^55^ caca ^111^")
                        .many(|m| m.first_of(|m| m.after("*")
                                                  .value::<u64>()
                                                  .const_str("*"),
                                             |m| m.after("^")
                                                  .value::<u64>()
                                                  .const_str("^")));

        assert_eq!(it.next(), Some(Left(44)));
        assert_eq!(it.next(), Some(Right(55)));
        assert_eq!(it.next(), Some(Right(111)));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn matcher_matches_char_class() {
        let mut it = matcher("+45 -32")
                        .many(|m| m.any(|m| m.white())
                                   .option(&[CharClass::Custom(&['-', '+'])])
                                   .merge::<u64,(char,u64),_>(|a, b| (a, b)));
        assert_eq!(it.next(), Some(('+', 45)));
        assert_eq!(it.next(), Some(('-', 32)));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn matcher_matches_different_types_of_chars() {
        let name = 
            matcher("fo43z &==% kaka")
                    .through(|m| m.option(&[CharClass::Letter, CharClass::Digit(10)]))
                    .white()
                    .result()
                    .unwrap();

        assert_eq!(name, "fo43z");
    }
}
