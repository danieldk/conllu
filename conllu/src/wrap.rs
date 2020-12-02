use std::borrow::Cow;
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::fmt;

use itertools::Itertools;
use std::ops::Deref;
use udgraph::graph::{DepGraph, Node, Sentence};
use udgraph::token::{Features, Misc};

use crate::error::ParseError;

#[derive(Debug, Eq, PartialEq)]
pub struct ConlluFeatures<'a>(pub Cow<'a, Features>);

impl<'a> ConlluFeatures<'a> {
    pub fn borrowed(features: &'a Features) -> Self {
        ConlluFeatures(Cow::Borrowed(features))
    }

    pub fn into_owned(self) -> Features {
        self.0.into_owned()
    }
}

impl ConlluFeatures<'static> {
    pub fn owned(features: Features) -> Self {
        ConlluFeatures(Cow::Owned(features))
    }

    fn parse_features(feature_string: impl AsRef<str>) -> Result<Self, ParseError> {
        let mut features = BTreeMap::new();

        if feature_string.as_ref() == "_" {
            return Ok(ConlluFeatures(Cow::Owned(Features::new())));
        }

        for fv in feature_string.as_ref().split('|') {
            let idx = fv.find('=').ok_or(ParseError::IncorrectFeatureField {
                value: fv.to_owned(),
            })?;

            features.insert(fv[..idx].to_owned(), fv[idx + 1..].to_owned());
        }

        Ok(ConlluFeatures::owned(features.into()))
    }
}

impl<'a> Deref for ConlluFeatures<'a> {
    type Target = BTreeMap<String, String>;

    fn deref(&self) -> &Self::Target {
        &**self.0
    }
}

impl<'a> fmt::Display for ConlluFeatures<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.is_empty() {
            write!(f, "_")
        } else {
            let features_str = self.0.iter().map(|(k, v)| format!("{}={}", k, v)).join("|");
            write!(f, "{}", features_str)
        }
    }
}

impl TryFrom<&str> for ConlluFeatures<'static> {
    type Error = ParseError;

    fn try_from(feature_string: &str) -> Result<Self, Self::Error> {
        Self::parse_features(feature_string)
    }
}

pub struct ConlluMisc<'a>(Cow<'a, Misc>);

impl<'a> ConlluMisc<'a> {
    pub fn borrowed(misc: &'a Misc) -> Self {
        ConlluMisc(Cow::Borrowed(misc))
    }

    pub fn into_owned(self) -> Misc {
        self.0.into_owned()
    }
}

impl ConlluMisc<'static> {
    pub fn owned(misc: Misc) -> Self {
        ConlluMisc(Cow::Owned(misc))
    }

    pub fn parse_misc(misc_string: impl AsRef<str>) -> BTreeMap<String, Option<String>> {
        let mut features = BTreeMap::new();

        for fv in misc_string.as_ref().split('|') {
            let fv: &str = fv;
            let (k, v) = fv
                .find('=')
                .map(|idx| (fv[..idx].to_owned(), Some(fv[idx + 1..].to_owned())))
                .unwrap_or_else(|| (fv.to_owned(), None));
            features.insert(k, v);
        }

        features
    }
}

impl<'a> fmt::Display for ConlluMisc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.is_empty() {
            write!(f, "_")
        } else {
            let misc_str = self
                .0
                .iter()
                .map(|(k, v)| match *v {
                    Some(ref v) => format!("{}={}", k, v),
                    None => k.to_owned(),
                })
                .join("|");
            write!(f, "{}", misc_str)
        }
    }
}

impl From<&str> for ConlluMisc<'static> {
    fn from(misc_string: &str) -> Self {
        ConlluMisc::owned(Self::parse_misc(misc_string).into())
    }
}

pub struct ConlluSentence<'a>(pub &'a Sentence);

impl<'a> fmt::Display for ConlluSentence<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for comment in self.0.comments() {
            writeln!(fmt, "{}", comment)?
        }

        for i in 1..self.0.len() {
            let token = match self.0[i] {
                Node::Token(ref token) => token,
                Node::Root => unreachable!(),
            };

            let (head, head_rel) = triple_to_string(&self.0.dep_graph(), i);

            writeln!(
                fmt,
                "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
                i,
                token.form(),
                token.lemma().unwrap_or("_"),
                token.upos().unwrap_or("_"),
                token.xpos().unwrap_or("_"),
                ConlluFeatures::borrowed(token.features()),
                head.unwrap_or_else(|| "_".to_string()),
                head_rel.unwrap_or_else(|| "_".to_string()),
                token.deps().unwrap_or("_"),
                ConlluMisc::borrowed(token.misc())
            )?;
        }

        Ok(())
    }
}

fn triple_to_string(g: &DepGraph, dependent: usize) -> (Option<String>, Option<String>) {
    //  XXX:return string reference for relation.
    let head_triple = g.head(dependent);
    let head = head_triple.as_ref().map(|t| t.head().to_string());
    let head_rel = head_triple
        .as_ref()
        .map(|t| t.relation().unwrap_or("_").to_string());

    (head, head_rel)
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::convert::TryFrom;
    use std::iter::FromIterator;

    use maplit::btreemap;
    use udgraph::token::{Features, Token, TokenBuilder};

    use crate::error::ParseError;
    use crate::wrap::ConlluFeatures;

    #[test]
    fn features_from_iter_as_string() {
        let feature_map = btreemap! {
            "feature2" => "y",
            "feature1" => "x"
        };

        let features = Features::from_iter(feature_map);
        let features_string: String = ConlluFeatures::borrowed(&features).to_string();

        assert_eq!(features_string, "feature1=x|feature2=y");

        assert_eq!(ConlluFeatures::owned(Features::new()).to_string(), "_");
    }

    #[test]
    fn features_with_colons() {
        let f = "Some=feature=with|additional=colons";
        let features = ConlluFeatures::try_from(f).unwrap();
        let some = features.get("Some").unwrap();
        assert_eq!(some, "feature=with");
        let additional = features.get("additional").unwrap();
        assert_eq!(additional, "colons");
    }

    #[test]
    fn feature_without_value_results_in_error() {
        assert_eq!(
            ConlluFeatures::try_from("c=d|a"),
            Err(ParseError::IncorrectFeatureField {
                value: "a".to_string()
            })
        );
    }

    #[test]
    fn eq_features_is_order_insensitive() {
        let token1: Token = TokenBuilder::new("a")
            .features(ConlluFeatures::try_from("a=b|c=d").unwrap().into_owned())
            .into();
        let token2 = TokenBuilder::new("a")
            .features(ConlluFeatures::try_from("c=d|a=b").unwrap().into_owned())
            .into();

        assert_eq!(token1, token2);
    }

    #[test]
    fn parse_empty_features() {
        assert_eq!(
            ConlluFeatures::try_from("_").unwrap().into_owned(),
            Features::new()
        );
    }

    #[test]
    fn features() {
        let tokens = token_with_features();
        let features = features_correct();

        for (token, correct) in tokens.iter().zip(features) {
            let kv = &**token.features();
            assert_eq!(&correct, kv);
        }
    }

    fn token_with_features() -> Vec<Token> {
        vec![
            TokenBuilder::new("Gilles")
                .lemma("Gilles")
                .upos("N")
                .xpos("NE")
                .features(
                    ConlluFeatures::try_from("case=nominative|number=singular|gender=masculine")
                        .unwrap()
                        .into_owned(),
                )
                .into(),
            TokenBuilder::new("Deleuze")
                .lemma("Deleuze")
                .upos("N")
                .xpos("NE")
                .features(
                    ConlluFeatures::try_from("case=nominative|number=singular|gender=masculine")
                        .unwrap()
                        .into_owned(),
                )
                .into(),
        ]
    }

    fn features_correct() -> Vec<BTreeMap<String, String>> {
        let mut correct = BTreeMap::new();
        correct.insert("case".to_owned(), "nominative".to_owned());
        correct.insert("number".to_owned(), "singular".to_owned());
        correct.insert("gender".to_owned(), "masculine".to_owned());

        vec![correct.clone(), correct]
    }
}
