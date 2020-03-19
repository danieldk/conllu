use std::convert::TryFrom;
use std::fs::File;
use std::io::BufReader;

use lazy_static::lazy_static;

use crate::graph::{DepTriple, Sentence};
use crate::io::{ReadSentence, Reader};
use crate::token::{Features, TokenBuilder};

lazy_static! {
    pub static ref TEST_SENTENCES: Vec<Sentence> = {
        let mut sentences = Vec::new();

        let mut s1 = Sentence::new();
        s1.push(
            TokenBuilder::new("Die")
                .lemma("die")
                .upos("ART")
                .xpos("ART")
                .features(
                    Features::try_from("case=nominative|gender=feminine|number=singular").unwrap(),
                )
                .into(),
        );

        s1.push(
            TokenBuilder::new("Großaufnahme")
                .lemma("Großaufnahme")
                .upos("N")
                .xpos("NN")
                .features(
                    Features::try_from("case=nominative|gender=feminine|number=singular").unwrap(),
                )
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
                .upos("N")
                .xpos("NE")
                .features(
                    Features::try_from("case=nominative|gender=masculine|number=singular").unwrap(),
                )
                .into(),
        );
        s2.push(
            TokenBuilder::new("Deleuze")
                .lemma("Deleuze")
                .upos("N")
                .xpos("NE")
                .features(
                    Features::try_from("case=nominative|gender=masculine|number=singular").unwrap(),
                )
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
