use std::fs::File;
use std::io::BufReader;

use conllu::io::{ReadSentence, Reader};
use udgraph::graph::Sentence;

pub fn read_sentences(filename: &str) -> Vec<Sentence> {
    Reader::new(BufReader::new(File::open(filename).unwrap()))
        .sentences()
        .map(|s| s.unwrap())
        .collect()
}
