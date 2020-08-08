// @TODO change to Cow<str> or &str if possible for later stages
#[derive(Clone, Debug, PartialEq)]
pub enum Lexeme {
    Text(String), // 'Text(..)' is parts of 'words' as defined in @POSIX 2
    Comment(String),
    Separator,

    // These cause 'output_index' and 'args_consumed' to reset to zero
    EndOfCommand,
    Pipe,
    EndOfBackgroundCommand,
    Break,
    Function(String),

    // Variables
    Variable(String),
    Private(usize, usize), // @TODO: Only for use in the parser

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
    Esac,
    EndIf,
    For,
    If,
    In,
    Then,
    Until,
    While,

    Debug(String),
}

#[derive(Debug)]
pub enum Parseme {
    Assign,
    Label(usize),
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


