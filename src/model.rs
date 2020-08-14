//run: cargo test model -- --nocapture

macro_rules! define_lexeme2 {
    (_level) => {
    };

    // Entry point
    (_enum $enum_name:ident : $repr:ty {
        $(
            $level:tt | $num_of_parsemes:literal |
                $variant:ident $(($data:ty))?
                    $(= $ref:ident = $discriminant:literal)?
        ,)*
    }) => {
        // Centralised place to change the type of the discriminant
        #[derive(Clone, Debug, PartialEq)]
        #[repr(u8)]
        pub enum $enum_name {
            $($variant $(($data))* $(= $discriminant)* ,)*
        }
        $($(const $ref: $repr = $discriminant;)*)*

        impl $enum_name {
            // Rust reference @PULL 639
            // @RFC 2363 @ISSUE 60553
            pub fn id(&self) -> $repr {
                unsafe { *(self as *const Self as *const $repr) }
            }
        }

        #[test]
        fn validate_named_discriminants2() {
            // Make sure our named discriminants ($ref) are matching associated
            // call to .id()
            $(
                // If no '$const_name' input, '_lexeme' will be not be used
                // hence the naming with an underscore
                let _lexeme = $enum_name::$variant $((<$data>::default() ))*;
                $( debug_assert_eq!(_lexeme.id(), $ref); )*
            )*
        }

    };

}

define_lexeme2! {
    _enum Lexeme2: u8 {
        - | 0 | Hello,
        + | 0 | Yo,
    }
}

// @TODO change to Cow<str> or &str if possible for later stages
// @TODO replace this with a proc macro
// @TODO https://github.com/landair/rust-proc-macro-without-dependencies
// Intended to be one-time use
macro_rules! define_lexeme {
    (_enum $enum_name:ident : $repr:ty { $(
        $variant:ident $(($data:ty))?  $(= $ref:ident = $discriminant:literal)?
    ,)*}) => {
        // @VOLATILE, change this to match $repr when used
        #[derive(Clone, Debug, PartialEq)]
        #[repr(u8)]
        pub enum $enum_name {
            $($variant $(($data))* $(= $discriminant)* ,)*
        }
        $($(const $ref: $repr = $discriminant;)*)*

        impl $enum_name {
            // Rust reference @PULL 639
            // @RFC 2363 @ISSUE 60553
            pub fn id(&self) -> $repr {
                unsafe { *(self as *const Self as *const $repr) }
            }
        }

        #[test]
        fn validate_named_discriminants() {
            // Make sure our named discriminants ($ref) are matching associated
            // call to .id()
            $(
                // If no '$const_name' input, '_x' will be not be used
                // hence the naming with an underscore
                let _x = $enum_name::$variant $((<$data>::default() ))*;
                $( debug_assert_eq!(_x.id(), $ref); )*
            )*
        }

    };
}

define_lexeme!{
    _enum Lexeme: u8 {
        // 'Text(..)' is parts of 'words' as defined in @POSIX 2
        Text(String) = LEXEME_TEXT = 0,
        Comment(String) = LEXEME_COMMENT = 1,
        Separator,

        // These cause 'output_index' and 'args_consumed' to reset to zero
        EndOfCommand,
        Pipe,
        EndOfBackgroundCommand,
        Break,
        Function(String) = LEXEME_FUNCTION = 8,

        // Variables
        Variable(String) = LEXEME_VARIABLE = 9,
        // @TODO: Only for use in the parser
        Private((usize, usize)) = LEXEME_PRIVATE = 10,

        // These deal with nesting
        ArithmeticStart,
        ArithmeticClose,
        SubShellStart,
        SubShellClose,
        ClosureStart,
        ClosureClose,
        HereDocStart,
        EndOfFile,

        OpInputHereDoc,
        OpInput,
        OpOutput,
        OpAssign,

        // Reserved words
        Case,
        Do,
        Done,
        ElseIf,
        Else,
        EndCase,
        EndIf,
        For,
        If,
        In,
        Then,
        Until,
        While,

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


