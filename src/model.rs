// @TODO change to Cow<str> or &str if possible for later stages
#[derive(Clone, Debug, PartialEq)]
pub enum Lexeme {
    Reserved(String),
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
    ArithmeticStart(usize),
    ArithmeticClose(usize),
    SubShellStart(usize),
    SubShellClose(usize),
    ClosureStart(usize),
    ClosureClose(usize),
    HereDocStart,
    EndOfFile,

    OpInputHereDoc,
    OpInput,
    OpOutput,
    OpAssign,

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

impl Executable {
    // @TODO Change to From<_>?
    //fn run(&self) {
    //    let mut args = self.args.iter();
    //    let mut cmd = Command::new(args.next().unwrap());
    //    cmd.args(args);
    //    //match &self.stdin {
    //    //    FileId::Descriptor(0) => cmd.stdin(io::stdin()),
    //    //    FileId::Descriptor(1) => cmd.stdin(io::stdout()),
    //    //    FileId::Descriptor(2) => cmd.stdin(io::stderr()),
    //    //    FileId::Descriptor(_) => &mut cmd,
    //    //    FileId::Path(_) => cmd.stdin(Stdio::piped()),
    //    //    FileId::Piped => cmd.stdin(Stdio::piped()),
    //    //};
    //}
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


