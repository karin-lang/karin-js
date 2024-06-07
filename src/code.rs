pub mod build;

#[derive(Clone, Debug, PartialEq)]
pub struct Code {
    pub source: String,
}

impl Code {
    pub fn new() -> Code {
        Code { source: String::new() }
    }

    pub fn append(&mut self, s: &str) {
        self.source += s;
    }
}
