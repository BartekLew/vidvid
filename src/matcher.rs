use std::str;

pub trait Matchable {
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

pub struct MatcherLoop<'a, R, F>
where
    F: Fn(Matcher<()>) -> Matcher<R>,
{
    tail: Option<&'a str>,
    f: F,
}

impl<'a, R, F> Iterator for MatcherLoop<'a, R, F>
where
    F: Fn(Matcher<()>) -> Matcher<R>,
{
    type Item = R;

    fn next(&mut self) -> Option<Self::Item> {
        let tail = self.tail.take();
        let m = (self.f)(matcher(tail?));
        self.tail = Some(m.tail);
        m.val
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
                println!("after: {}", self.tail);
                return self;
            }
            finder.tail = &finder.tail[1..];
            finder.val = Some(());
        }

        self.val = None;
        return self;
    }

    #[cfg(test)]
    fn until_word(self) -> Matcher<'a, &'a str> {
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
    fn until<R: Matchable>(self) -> Matcher<'a, <R as Matchable>::Out> {
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
            where F: for <'b> Fn(Matcher<'b, ()>) -> Matcher<'b, R> {
        MatcherLoop { tail: Some(self.tail), f }
    }

    pub fn skip_many<R,F>(mut self, f:F) -> Self 
            where F: for <'b> Fn(Matcher<'b, ()>) -> Matcher<'b, R> {
        let mut it = MatcherLoop { tail: Some(self.tail), f };
        while it.next().is_some() {
            self.tail = it.tail.unwrap();
        }
        self
    }

    #[cfg(test)]
    fn word(self) -> Matcher<'a, &'a str> {
        let mut i: usize = 0;
        let mut cur = self.tail.chars();
        while cur.next().map(|c| c.is_alphabetic()).unwrap_or(false) {
            i += 1;
        }

        if i > 0 {
            Matcher { tail: &self.tail[i..], val: Some(&self.tail[0..i]) }
        } else {
            Matcher { tail: self.tail, val: None }
        }
    }

    pub fn white(mut self) -> Self {
        match self.tail.chars().next() {
            Some(c) => if c.is_whitespace() {
                            self.tail = &self.tail[1..];
                            return self
                       },
            None => {}
        }

        self.val = None;
        self
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

    #[cfg(test)]
    pub fn result(self) -> Option<T> {
        self.val
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
    fn matcher_matches_many () {
        let mut it = matcher("foo=1 bar=2 zza=3")
                       .many(|m1|
                             m1.until_word()
                               .const_str("=")
                               .merge::<u64,(String, u64),_>(|name, val| (name.to_owned(),val)));

        assert_eq!(it.next(), Some(("foo".to_owned(),1)));
        assert_eq!(it.next(), Some(("bar".to_owned(),2)));
        assert_eq!(it.next(), Some(("zza".to_owned(),3)));
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
}
