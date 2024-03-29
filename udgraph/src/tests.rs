use std::iter::FromIterator;

use lazy_static::lazy_static;
use maplit::btreemap;

use crate::graph::{Comment, DepTriple, Sentence};
use crate::token::{Misc, Token, TokenBuilder};

lazy_static! {
    pub static ref TEST_SENTENCES: Vec<Sentence> = {
        let mut sentences = Vec::new();

        let mut s1 = Sentence::new();
        s1.comments_mut().push(Comment::AttrVal {
            attr: "sent_id".to_string(),
            val: "1".to_string(),
        });
        s1.comments_mut()
            .push(Comment::String("some random comment".to_string()));
        s1.push(
            TokenBuilder::new("Die")
                .lemma("die")
                .upos("ART")
                .xpos("ART")
                .features(
                    btreemap! {
                        "case".to_string() => "nominative".to_string(),
                        "gender".to_string() => "feminine".to_string(),
                        "number".to_string() => "singular".to_string(),
                    }
                    .into(),
                )
                .deps("2:det")
                .misc(Misc::from_iter(vec![
                    ("misc1", None),
                    ("misc2", Some("value")),
                ]))
                .into(),
        );

        s1.push(
            TokenBuilder::new("Großaufnahme")
                .lemma("Großaufnahme")
                .upos("N")
                .xpos("NN")
                .features(
                    btreemap! {
                        "case".to_string() => "nominative".to_string(),
                        "gender".to_string() => "feminine".to_string(),
                        "number".to_string() => "singular".to_string(),
                    }
                    .into(),
                )
                .into(),
        );

        s1.dep_graph_mut()
            .add_deprel(DepTriple::new(2, Some("DET"), 1))
            .unwrap();
        s1.dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("ROOT"), 2))
            .unwrap();

        sentences.push(s1);

        let mut s2 = Sentence::new();
        s2.push(
            TokenBuilder::new("Gilles")
                .lemma("Gilles")
                .upos("N")
                .xpos("NE")
                .features(
                    btreemap! {
                        "case".to_string() => "nominative".to_string(),
                        "gender".to_string() => "masculine".to_string(),
                        "number".to_string() => "singular".to_string(),
                    }
                    .into(),
                )
                .misc(btreemap! { "NE".to_string() => Some("per".to_string()) }.into())
                .into(),
        );
        s2.push(
            TokenBuilder::new("Deleuze")
                .lemma("Deleuze")
                .upos("N")
                .xpos("NE")
                .features(
                    btreemap! {
                        "case".to_string() => "nominative".to_string(),
                        "gender".to_string() => "masculine".to_string(),
                        "number".to_string() => "singular".to_string()
                    }
                    .into(),
                )
                .misc(btreemap! { "NE".to_string() => Some("per".to_string()) }.into())
                .into(),
        );
        s2.dep_graph_mut()
            .add_deprel(DepTriple::new(0, Some("ROOT"), 1))
            .unwrap();
        s2.dep_graph_mut()
            .add_deprel(DepTriple::new(1, Some("APP"), 2))
            .unwrap();
        sentences.push(s2);

        let mut s3 = Sentence::new();
        s3.push(Token::new("Plain"));
        s3.push(Token::new("and"));
        s3.push(Token::new("simple"));
        sentences.push(s3);

        let mut s4 = Sentence::new();
        s4.push(
            TokenBuilder::new("Amsterdam")
                .misc(btreemap! { "NE".to_string() => Some("loc".to_string()) }.into())
                .into(),
        );
        eprintln!("{:?}", s4);
        sentences.push(s4);

        sentences
    };
}
