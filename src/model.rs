//run: cargo test model -- --nocapture

// @TODO change to Cow<str> or &str if possible for later stages
// @TODO replace this with a proc macro
// @TODO https://github.com/landair/rust-proc-macro-without-dependencies
// Intended to be one-time use
macro_rules! define_lexeme {
    (@level -) => { -1 };
    (@level =) => { 0 };
    (@level +) => { 1 };
    (@enum $enum_name:ident : $repr:ty {
        $(
            $($num_of_parsemes:literal | $level:tt | )?
                $variant:ident $(($data:ty))?
                    $(= $named_discriminant:ident = $discriminant:literal)?
        ,)*
    }) => {
        // @VOLATILE, change this to match $repr when used
        #[derive(Clone, Debug, PartialEq)]
        #[repr(u8)]
        pub enum $enum_name {
            $($variant $(($data))* $(= $discriminant)* ,)*
        }
        $($(const $named_discriminant: $repr = $discriminant;)*)*

        impl $enum_name {
            // Rust reference @PULL 639
            // @RFC 2363 @ISSUE 60553
            #[inline]
            pub fn id(&self) -> $repr {
                unsafe { *(self as *const Self as *const $repr) }
            }
        }

        pub const LEXEME_PARSEME_COUNT: [u8; LEXEME_OPTIONS_COUNT as usize] = {
            let mut temp = [0; LEXEME_OPTIONS_COUNT as usize];
            let mut _i = 0;
            $(
                $(temp[_i] = $num_of_parsemes;)*
                _i += 1;
            )*
            temp
        };

        pub const LEXEME_LEVEL: [i8; LEXEME_OPTIONS_COUNT as usize] = {
            let mut temp = [0; LEXEME_OPTIONS_COUNT as usize];
            let mut i: usize = 0;
            $(
                $(temp[i] = define_lexeme!(@level $level);)*
                i += 1;
            )*
            temp
        };

        // @TODO: these tests can go away if we do a proc macro
        #[test]
        fn validate_named_discriminants() {
            let mut i = 0;

            // Make sure our named discriminants are matching associated
            // call to .id()
            $(
                let x = $enum_name::$variant $((<$data>::default() ))*;
                // Only enumerates if discriminant was named
                $( assert_eq!(x.id(), $named_discriminant); )*

                // Discriminants are the defaults (sequentially + 1 from 0)
                assert_eq!(x.id(), {
                    i += 1;
                    i - 1
                });
            )*

        }

    };
}

//#[test]
//fn asdf() {
//    println!("{:?}", Lexeme::Pipe as usize);
//}

pub type LexemeRepr = u8;

define_lexeme! {
    @enum Lexeme: LexemeRepr {
        // 'Text(..)' is parts of 'words' as defined in @POSIX 2
        0 | = | Text(String) = LEXEME_TEXT = 0,
        0 | = | Comment(String) = LEXEME_COMMENT = 1,
        0 | = | Separator,

        // These cause 'output_index' and 'args_consumed' to reset to zero
        1 | = | EndOfCommand,
        1 | = | Pipe,
        1 | = | EndOfBackgroundCommand,
        1 | = | Break,

        // Variables
        0 | = | Function(String) = LEXEME_FUNCTION = 7,
        0 | = | Variable(String) = LEXEME_VARIABLE = 8,
        // @TODO: Only for use in the parser
        0 | = | Private((usize, usize)) = LEXEME_PRIVATE = 9,

        // These deal with nesting
        0 | + | ArithmeticStart,
        1 | - | ArithmeticClose,
        0 | + | SubShellStart,
        1 | - | SubShellClose,
        0 | + | ClosureStart,
        1 | - | ClosureClose,
        0 | + | HereDocStart,
        0 | - | EndOfFile,

        0 | = | OpInputHereDoc,
        0 | = | OpInput,
        0 | = | OpOutput,
        0 | = | OpAssign,

        // Reserved words
        1 | + | Case,
        1 | - | EndCase,
        1 | = | Do, // Level increase handled by Lexeme::While/Until/For
        0 | - | Done, // count = 0 because Lexeme::EndOfCommand follows
        1 | = | ElseIf,
        1 | = | Else,
        1 | - | EndIf,
        1 | + | For,
        1 | + | If,
        0 | = | In,
        1 | - | Then, // Level increase handled by Lexeme::If
        1 | + | Until,
        1 | + | While,

        Size = LEXEME_OPTIONS_COUNT = 35,
        Debug(String),
    }
}


#[derive(Debug)]
pub enum Parseme {
    Assign,
    Label(usize, usize),
    JumpIfFalse(usize),
    JumpIfTrue(usize),
    Jump(usize),
    External(Executable),
    Debug(Vec<Lexeme>),
}

//#[derive(Debug)]
pub struct Executable {
    pub args: Vec<Lexeme>,
    pub handles: IoHandles,
}

impl std::fmt::Debug for Executable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Executable({:?}    <{} >{} 2>{})",
            self.args,
            match &self.handles.stdin {
                FileId::Descriptor(0) => "&0".to_string(),
                FileId::Descriptor(1) => "&1".to_string(),
                FileId::Descriptor(2) => "&2".to_string(),
                FileId::Path(s) => format!("{:?}", s),
                _ => format!("{:?}", self.handles.stdin),
            },
            match &self.handles.stdout {
                FileId::Descriptor(0) => "&0".to_string(),
                FileId::Descriptor(1) => "&1".to_string(),
                FileId::Descriptor(2) => "&2".to_string(),
                FileId::Path(s) => format!("{:?}", s),
                _ => format!("{:?}", self.handles.stdout),
            },
            match &self.handles.stderr {
                FileId::Descriptor(0) => "&0".to_string(),
                FileId::Descriptor(1) => "&1".to_string(),
                FileId::Descriptor(2) => "&2".to_string(),
                FileId::Path(s) => format!("{:?}", s),
                _ => format!("{:?}", self.handles.stderr),
            },
        )
    }
}

// Representation of file descriptors
#[derive(Clone, Debug)]
pub enum FileId {
    Descriptor(usize),
    Path(Word),
    PublicVariable(String),
    PrivateVariable(usize, usize),
    Piped,
}

type Word = Vec<Lexeme>; // word as in @POSIX 2

// @TODO @POSIX 2.7 Redirection says at least [0,9] shall be supported
// Maybe an associated array or an array of (descripto
#[derive(Clone, Debug)]
pub struct IoHandles {
    pub stdin: FileId,
    pub stdout: FileId,
    pub stderr: FileId,
}

impl IoHandles {
    pub fn new() -> Self {
        Self {
            stdin: FileId::Descriptor(0),
            stdout: FileId::Descriptor(1),
            stderr: FileId::Descriptor(2),
        }
    }
}
