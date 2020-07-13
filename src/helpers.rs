//run: cargo test -- --nocapture

pub struct TextGridWalk<'a> {
//pub struct TextGridWalk<'a> {
    peek: Option<GridItem<'a>>,

    cur_line: &'a str,
    row: usize,
    col: usize,
    index: usize,
    line_source: SplitInclusive<'a>,
    char_source: std::str::Chars<'a>,
}

//type GridItem<'a> = (usize, char, (&'a str, usize, usize));
type GridItem<'a> = (&'a str, usize, char, (usize, usize));
impl<'a> TextGridWalk<'a> {
    pub fn new(buffer: &'a str) -> Self {
        let mut char_source = buffer.chars();
        let mut line_source = split_before_newlines(buffer);
        let cur_line = line_source.next().unwrap_or("");
        let col = 1;
        let row = 1;
        Self {
            peek: char_source.next().map(|ch| (cur_line, 0, ch, (row, col))),

            cur_line,
            row,
            col,
            index: 0,
            line_source,
            char_source,
        }
    }

    /// Peek is not lazy like std::iter::peekable().peek()
    /// The first peek is evaluated eagerly on 'new()'
    pub fn peek(&self) -> Option<<Self as Iterator>::Item> {
        self.peek
    }

    /// NOTE: Unlike 'peek()' this calls 'next()' which means it will progress
    /// the iterator
    pub fn peek_while<F>(&mut self, predicate: F) -> Option<usize>
    where
        F: Fn(char) -> bool
    {
        loop {
            if let Some((_, index, c, _)) = self.peek {
                if !predicate(c) {
                    break Some(index);
                }
            } else {
                break None;
            }
            self.next();  // 'peek()' and 'next()' are 'None' at the same time
        }
    }

    pub fn next_till<F>(&mut self, predicate: F) -> Option<usize>
    where
        F: Fn(char) -> bool
    {
        loop {
            if let Some((_, index, c, _)) = self.next() {
                if predicate(c) {
                    break Some(index);
                }
            } else {
                break None;
            }
        }
    }
}

impl<'a> Iterator for TextGridWalk<'a> {
    type Item = GridItem<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.peek.map(|cur| {
            self.peek = self.char_source.next().map(|ch| {
                self.index += cur.2.len_utf8();
                if ch == '\n' {
                    self.cur_line = self.line_source.next().unwrap_or("");
                    self.row += 1;
                    self.col = 0;
                } else {
                    self.col += 1;
                }
                //(self.index, ch, (self.cur_line, self.row, self.col))
                (self.cur_line, self.index, ch, (self.row, self.col))
            });
            cur
        })
    }
}

#[test]
fn grid_walk_test() {
    let buffer = "你好號碼hello\ndarkness別忘了我my ol\nd朋友a阿a";
    let walker1 = buffer.chars();
    let mut walker2 = TextGridWalk::new(buffer);

    walker1.for_each(|ch1| {
        let (_, index, ch2, _) = walker2.next().unwrap();
        assert_eq!(ch1, ch2);
        assert_eq!(Some(ch1), buffer[index..].chars().next());
    });
    //TextGridWalk::new(buffer).for_each(|tuple| println!("{:?}", tuple));
}

//#[derive(Debug)]
//pub struct SplitInclusive<'a, F>(&'a str, F);
pub struct SplitInclusive<'a>(&'a str, &'a dyn Fn(&'a str) -> Option<usize>);


//impl<'a, F> Iterator for SplitInclusive<'a, F>
//where
    //F: Fn(&'a str) -> Option<usize>,
impl<'a> Iterator for SplitInclusive<'a>
{
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        let Self(rest, pattern) = self;
        let index = pattern(*rest).or_else(|| {
            if rest.is_empty() {
                None
            } else {
                Some(rest.len()) // So that 'split_at()' splits everything
            }
        });
        index.map(|i| {
            let (to_return, to_process) = rest.split_at(i);
            *rest = to_process;
            to_return
        })
    }
}

#[test]
pub fn hello() {
    let text = "你好號碼hello\ndarkness\nd朋友a阿a";
    SplitInclusive(text, &|rest: &str| {
        let mut as_chars = rest.chars();
        as_chars.next().and_then(|ch| {
            as_chars.as_str().find('\n').map(|i| ch.len_utf8() + i)
        })
    })
    .for_each(|line| println!("|{:?}|", line));
}

pub fn split_before_newlines<'a>(buffer: &'a str) -> SplitInclusive<'a> {
    SplitInclusive(buffer, &|rest: &str| {
        let mut as_chars = rest.chars();
        as_chars.next().and_then(|ch| {
            as_chars.as_str().find('\n').map(|i| ch.len_utf8() + i)
        })
    })
}

fn split_after_newlines<'a>(buffer: &'a str) -> impl Iterator<Item = &'a str> {
    SplitInclusive(buffer, &|rest: &'a str| rest.find('\n').map(|i| i + 1))
}


#[test]
fn splitting_inclusively() {
    let text = "foo\r\nbar\n\nbaz\n";
    let mut lines = split_after_newlines(text);
    assert_eq!(Some("foo\r\n"), lines.next());
    assert_eq!(Some("bar\n"), lines.next());
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(Some("baz\n"), lines.next());
    assert_eq!(None, lines.next());
    let mut lines = split_before_newlines(text);
    assert_eq!(Some("foo\r"), lines.next());
    assert_eq!(Some("\nbar"), lines.next());
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(Some("\nbaz"), lines.next());
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(None, lines.next());

    let text = "\n\n";
    let mut lines = split_after_newlines(text);
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(None, lines.next());
    let mut lines = split_before_newlines(text);
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(None, lines.next());

    let text = "\n";
    let mut lines = split_after_newlines(text);
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(None, lines.next());
    let mut lines = split_before_newlines(text);
    assert_eq!(Some("\n"), lines.next());
    assert_eq!(None, lines.next());

    let text = "";
    let mut lines = split_after_newlines(text);
    assert_eq!(None, lines.next());
    let mut lines = split_before_newlines(text);
    assert_eq!(None, lines.next());

    let text = "hello";
    let mut lines = split_after_newlines(text);
    assert_eq!(Some("hello"), lines.next());
    assert_eq!(None, lines.next());
    let mut lines = split_before_newlines(text);
    assert_eq!(Some("hello"), lines.next());
    assert_eq!(None, lines.next());

    let text = "hello\ndarkness";
    let mut lines = split_after_newlines(text);
    assert_eq!(Some("hello\n"), lines.next());
    assert_eq!(Some("darkness"), lines.next());
    assert_eq!(None, lines.next());
    let mut lines = split_before_newlines(text);
    assert_eq!(Some("hello"), lines.next());
    assert_eq!(Some("\ndarkness"), lines.next());
    assert_eq!(None, lines.next());

    let text = "你好號碼hello\ndarkness別忘了我my ol\nd朋友a阿a";
    let mut lines = split_before_newlines(text);
    assert_eq!(Some("你好號碼hello"), lines.next());
    assert_eq!(Some("\ndarkness別忘了我my ol"), lines.next());
    assert_eq!(Some("\nd朋友a阿a"), lines.next());
    assert_eq!(None, lines.next());
}

// @CLEANUP this is in nightly as then_some()
pub trait OwnedToOption<T> {
    fn to_some(self, val: T) -> Option<T>;
}

impl<T> OwnedToOption<T> for bool {
    fn to_some(self, val: T) -> Option<T> {
        if self {
            Some(val)
        } else {
            None
        }
    }
}
