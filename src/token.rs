//! Tokens in the dependency graph.

use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Display;
use std::iter::FromIterator;
use std::mem;
use std::ops::{Deref, DerefMut};

use itertools::Itertools;

pub const EMPTY_TOKEN: &str = "_";

/// A builder for `Token`s.
///
/// The `Token` type stores a CoNLL-U token. However, since this format
/// permits a large number of fields, construction of a token can get
/// tedious. This builder provides a fluent interface for creating `Token`s.
pub struct TokenBuilder {
    token: Token,
}

impl TokenBuilder {
    /// Create a `Token` builder with all non-form fields set to absent.
    pub fn new(form: impl Into<String>) -> TokenBuilder {
        TokenBuilder {
            token: Token::new(form),
        }
    }

    /// Set the word form or punctuation symbol.
    pub fn form(mut self, form: impl Into<String>) -> TokenBuilder {
        self.token.set_form(form);
        self
    }

    /// Set the lemma or stem of the word form.
    pub fn lemma(mut self, lemma: impl Into<String>) -> TokenBuilder {
        self.token.set_lemma(Some(lemma));
        self
    }

    /// Set the coarse-grained part-of-speech tag.
    pub fn cpos(mut self, cpos: impl Into<String>) -> TokenBuilder {
        self.token.set_cpos(Some(cpos));
        self
    }

    /// Set the fine-grained part-of-speech tag.
    pub fn pos(mut self, pos: impl Into<String>) -> TokenBuilder {
        self.token.set_pos(Some(pos));
        self
    }

    /// Set the syntactic and/or morphological features of the token.
    pub fn features(mut self, features: Features) -> TokenBuilder {
        self.token.set_features(Some(features));
        self
    }
}

impl From<Token> for TokenBuilder {
    fn from(token: Token) -> Self {
        TokenBuilder { token }
    }
}

impl From<TokenBuilder> for Token {
    fn from(builder: TokenBuilder) -> Self {
        builder.token
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    form: String,
    lemma: Option<String>,
    cpos: Option<String>,
    pos: Option<String>,
    features: Option<Features>,
}

impl Token {
    /// Create a new token where all the non-form fields are absent.
    pub fn new(form: impl Into<String>) -> Token {
        Token {
            form: form.into(),
            lemma: None,
            cpos: None,
            pos: None,
            features: None,
        }
    }

    /// Get the word form or punctuation symbol.
    pub fn form(&self) -> &str {
        self.form.as_ref()
    }

    /// Get the lemma or stem of the word form.
    pub fn lemma(&self) -> Option<&str> {
        self.lemma.as_ref().map(String::as_ref)
    }

    /// Get the coarse-grained part-of-speech tag.
    pub fn cpos(&self) -> Option<&str> {
        self.cpos.as_ref().map(String::as_ref)
    }

    /// Get the fine-grained part-of-speech tag.
    pub fn pos(&self) -> Option<&str> {
        self.pos.as_ref().map(String::as_ref)
    }

    /// Get the syntactic and/or morphological features of the token.
    pub fn features(&self) -> Option<&Features> {
        self.features.as_ref()
    }

    /// Get the syntactic and/or morphological features of the token.
    ///
    /// Returns a mutable reference, so that the features can be updated.
    pub fn features_mut(&mut self) -> Option<&mut Features> {
        self.features.as_mut()
    }

    /// Set the word form or punctuation symbol.
    ///
    /// Returns the form that is replaced.
    pub fn set_form(&mut self, form: impl Into<String>) -> String {
        mem::replace(&mut self.form, form.into())
    }

    /// Set the lemma or stem of the word form.
    ///
    /// Returns the lemma that is replaced.
    pub fn set_lemma<S>(&mut self, lemma: Option<S>) -> Option<String>
    where
        S: Into<String>,
    {
        mem::replace(&mut self.lemma, lemma.map(Into::into))
    }

    /// Set the coarse-grained part-of-speech tag.
    ///
    /// Returns the coarse-grained part-of-speech tag that is replaced.
    pub fn set_cpos<S>(&mut self, cpos: Option<S>) -> Option<String>
    where
        S: Into<String>,
    {
        mem::replace(&mut self.cpos, cpos.map(Into::into))
    }

    /// Set the fine-grained part-of-speech tag.
    ///
    /// Returns the fine-grained part-of-speech tag that is replaced.
    pub fn set_pos<S>(&mut self, pos: Option<S>) -> Option<String>
    where
        S: Into<String>,
    {
        mem::replace(&mut self.pos, pos.map(Into::into))
    }

    /// Set the syntactic and/or morphological features of the token.
    ///
    /// Returns the features that are replaced.
    pub fn set_features(&mut self, features: Option<Features>) -> Option<Features> {
        mem::replace(&mut self.features, features)
    }
}

/// Token features.
///
/// In the CoNLL-U specification, these are morphological features of the
/// token. Typically, the features are a list or a key-value mapping.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Features {
    inner: BTreeMap<String, Option<String>>,
}

impl Features {
    /// Construct an empty set of features.
    pub fn new() -> Self {
        Features {
            inner: BTreeMap::new(),
        }
    }

    /// Unwrap the contained feature map.
    pub fn into_inner(self) -> BTreeMap<String, Option<String>> {
        self.inner
    }

    fn parse_features(feature_string: impl AsRef<str>) -> BTreeMap<String, Option<String>> {
        let mut features = BTreeMap::new();

        for fv in feature_string.as_ref().split('|') {
            let fv: &str = fv;
            let (k, v) = fv
                .find(':')
                .map(|idx| (fv[..idx].to_owned(), Some(fv[idx + 1..].to_owned())))
                .unwrap_or_else(|| (fv.to_owned(), None));
            features.insert(k, v);
        }

        features
    }
}

impl Default for Features {
    fn default() -> Self {
        Features::new()
    }
}

impl Deref for Features {
    type Target = BTreeMap<String, Option<String>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Features {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Display for Features {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let feature_str: String = self.into();
        f.write_str(&feature_str)
    }
}

impl From<BTreeMap<String, Option<String>>> for Features {
    fn from(feature_map: BTreeMap<String, Option<String>>) -> Self {
        Features { inner: feature_map }
    }
}

impl From<&str> for Features {
    fn from(feature_string: &str) -> Self {
        Features {
            inner: Features::parse_features(feature_string),
        }
    }
}

impl<S, T> FromIterator<(S, Option<T>)> for Features
where
    S: Into<String>,
    T: Into<String>,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (S, Option<T>)>,
    {
        let features =
            BTreeMap::from_iter(iter.into_iter().map(|(k, v)| (k.into(), v.map(Into::into))));

        Features { inner: features }
    }
}

impl From<Features> for String {
    fn from(features: Features) -> Self {
        (&features).into()
    }
}

impl From<&Features> for String {
    fn from(features: &Features) -> Self {
        features
            .inner
            .iter()
            .map(|(k, v)| match *v {
                Some(ref v) => format!("{}:{}", k, v),
                None => k.to_owned(),
            })
            .join("|")
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::iter::FromIterator;

    use maplit::btreemap;
    use quickcheck::quickcheck;

    use super::{Features, Token, TokenBuilder};

    quickcheck! {
        fn features_from_iter(feature_map: BTreeMap<String, Option<String>>) -> bool{
            feature_map == *Features::from_iter(feature_map.clone())
        }
    }

    #[test]
    fn features_from_iter_as_string() {
        let feature_map = btreemap! {
            "feature2" => Some("y"),
            "feature3" => None,
            "feature1" => Some("x")
        };

        let features = Features::from_iter(feature_map);
        let features_string: String = features.into();

        assert_eq!(features_string, "feature1:x|feature2:y|feature3");
    }

    #[test]
    fn features_with_colons() {
        let f = "Some:feature:with|additional:colons|feature";
        let features = Features::from(f);
        let some = features.get("Some").unwrap().as_ref().map(String::as_str);
        assert_eq!(some, Some("feature:with"));
        let additional = features
            .get("additional")
            .unwrap()
            .as_ref()
            .map(String::as_str);
        assert_eq!(additional, Some("colons"));
        let feature = features
            .get("feature")
            .unwrap()
            .as_ref()
            .map(String::as_str);
        assert_eq!(feature, None);
    }

    #[test]
    fn features() {
        let tokens = token_with_features();
        let features = features_correct();

        for (token, correct) in tokens.iter().zip(features) {
            let kv = &**token.features().unwrap();
            assert_eq!(&correct, kv);
        }
    }

    fn token_with_features() -> Vec<Token> {
        vec![
            TokenBuilder::new("Gilles")
                .lemma("Gilles")
                .cpos("N")
                .pos("NE")
                .features(Features::from(
                    "case:nominative|number:singular|gender:masculine",
                ))
                .into(),
            TokenBuilder::new("Deleuze")
                .lemma("Deleuze")
                .cpos("N")
                .pos("NE")
                .features(Features::from("nominative|singular|masculine"))
                .into(),
        ]
    }

    fn features_correct() -> Vec<BTreeMap<String, Option<String>>> {
        let mut correct1 = BTreeMap::new();
        correct1.insert("case".to_owned(), Some("nominative".to_owned()));
        correct1.insert("number".to_owned(), Some("singular".to_owned()));
        correct1.insert("gender".to_owned(), Some("masculine".to_owned()));

        let mut correct2 = BTreeMap::new();
        correct2.insert("nominative".to_owned(), None);
        correct2.insert("singular".to_owned(), None);
        correct2.insert("masculine".to_owned(), None);

        vec![correct1, correct2]
    }

    #[test]
    fn eq_features_is_order_insensitive() {
        let token1: Token = TokenBuilder::new("a")
            .features(Features::from("a|b:c"))
            .into();
        let token2 = TokenBuilder::new("a")
            .features(Features::from("b:c|a"))
            .into();
        let token3: Token = TokenBuilder::new("a")
            .features(Features::from("b|a:c"))
            .into();

        assert_eq!(token1, token2);
        assert_ne!(token1, token3);
        assert_ne!(token2, token3);
    }
}
