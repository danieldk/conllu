#![feature(test)]

extern crate conllx;

extern crate test;

use test::{black_box, Bencher};

use conllx::token::Features;

static FEATURES: &'static [&'static str] = &[
    "cat:regular+noun|case:nominative|number:plural",
    "cat:article|definite:true|case:nominative|number:singular|gender:feminine",
    "cat:proper+name|case:nominative|number:singular|gender:feminine",
    "cat:regular+noun|case:nominative|number:singular|case:neuter",
    "cat:symbol|cat:punctuation",
];

#[bench]
pub fn bench_as_map(b: &mut Bencher) {
    b.iter(|| {
        for f_str in FEATURES {
            let features = Features::from_string(*f_str);
            black_box(features.as_map());
            black_box(features.as_map());
            black_box(features.as_map());
            black_box(features.as_map());
        }
    });
}
