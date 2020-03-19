use std::fs::File;
use std::io::BufReader;

use lazy_static::lazy_static;

use crate::conllx::io::{ReadSentence, Reader};
use crate::conllx::token::{Features, TokenBuilder};
use crate::graph::{DepTriple, Sentence};

lazy_static! {
    pub static ref TEST_SENTENCES: Vec<Sentence> = {
        let mut sentences = Vec::new();

        let mut s1 = Sentence::new();
        s1.push(
            TokenBuilder::new("Die")
                .lemma("die")
                .cpos("ART")
                .pos("ART")
                .features(Features::from("nsf"))
                .into(),
        );

        s1.push(
            TokenBuilder::new("Großaufnahme")
                .lemma("Großaufnahme")
                .cpos("N")
                .pos("NN")
                .features(Features::from("nsf"))
                .into(),
        );

        s1.dep_graph_mut()
            .add_deprel(DepTriple::new(2, Some("DET"), 1));
        s1.dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("ROOT"), 2));
        s1.proj_dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("TEST"), 1));

        sentences.push(s1);

        let mut s2 = Sentence::new();
        s2.push(
            TokenBuilder::new("Gilles")
                .lemma("Gilles")
                .cpos("N")
                .pos("NE")
                .features(Features::from("nsm"))
                .into(),
        );
        s2.push(
            TokenBuilder::new("Deleuze")
                .lemma("Deleuze")
                .cpos("N")
                .pos("NE")
                .features(Features::from(
                    "case:nominative|gender:masculine|number:singular",
                ))
                .into(),
        );
        s2.dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("ROOT"), 1));
        s2.dep_graph_mut()
            .add_deprel(DepTriple::new(1, Some("APP"), 2));
        sentences.push(s2);

        sentences
    };
}

pub fn read_sentences(filename: &str) -> Vec<Sentence> {
    Reader::new(BufReader::new(File::open(filename).unwrap()))
        .sentences()
        .map(|s| s.unwrap())
        .collect()
}
