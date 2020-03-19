//! CoNLL-U tokens.

use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::Display;
use std::iter::FromIterator;
use std::mem;
use std::ops::{Deref, DerefMut};

use failure::Fail;
use itertools::Itertools;

pub const EMPTY_TOKEN: &str = "_";

/// A builder for `Token`s.
///
/// The `Token` type stores a CoNLL-X token. However, since this format
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

    /// Set the universal part-of-speech tag.
    pub fn upos(mut self, upos: impl Into<String>) -> TokenBuilder {
        self.token.set_upos(Some(upos));
        self
    }

    /// Set the language-specific part-of-speech tag.
    pub fn xpos(mut self, xpos: impl Into<String>) -> TokenBuilder {
        self.token.set_xpos(Some(xpos));
        self
    }

    /// Set the syntactic and/or morphological features of the token.
    pub fn features(mut self, features: Features) -> TokenBuilder {
        self.token.set_features(Some(features));
        self
    }

    /// Set the additional information associated with the token.
    pub fn misc(mut self, misc: impl Into<Vec<String>>) -> TokenBuilder {
        self.token.set_misc(Some(misc));
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
    upos: Option<String>,
    xpos: Option<String>,
    features: Option<Features>,
    misc: Option<Vec<String>>,
}

impl Token {
    /// Create a new token where all the non-form fields are absent.
    pub fn new(form: impl Into<String>) -> Token {
        Token {
            form: form.into(),
            lemma: None,
            upos: None,
            xpos: None,
            features: None,
            misc: None,
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

    /// Get additional information associated with the token.
    pub fn misc(&self) -> Option<&[String]> {
        self.misc.as_deref()
    }

    /// Get additional information associated with the token mutably.
    pub fn misc_mut(&mut self) -> Option<&mut Vec<String>> {
        self.misc.as_mut()
    }

    /// Get the universal part-of-speech tag.
    pub fn upos(&self) -> Option<&str> {
        self.upos.as_ref().map(String::as_ref)
    }

    /// Get the language-specific part-of-speech tag.
    pub fn xpos(&self) -> Option<&str> {
        self.xpos.as_ref().map(String::as_ref)
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
    pub fn set_lemma(&mut self, lemma: Option<impl Into<String>>) -> Option<String> {
        mem::replace(&mut self.lemma, lemma.map(Into::into))
    }

    /// Set the additional information associated with the token.
    ///
    /// Returns the information that is replaced.
    pub fn set_misc(&mut self, misc: Option<impl Into<Vec<String>>>) -> Option<Vec<String>> {
        mem::replace(&mut self.misc, misc.map(Into::into))
    }

    /// Set the universal part-of-speech tag.
    ///
    /// Returns the universal part-of-speech tag that is replaced.
    pub fn set_upos(&mut self, upos: Option<impl Into<String>>) -> Option<String> {
        mem::replace(&mut self.upos, upos.map(Into::into))
    }

    /// Set the language-specific part-of-speech tag.
    ///
    /// Returns the language-specific part-of-speech tag that is replaced.
    pub fn set_xpos(&mut self, xpos: Option<impl Into<String>>) -> Option<String> {
        mem::replace(&mut self.xpos, xpos.map(Into::into))
    }

    /// Set the syntactic and/or morphological features of the token.
    ///
    /// Returns the features that are replaced.
    pub fn set_features(&mut self, features: Option<Features>) -> Option<Features> {
        mem::replace(&mut self.features, features)
    }
}

#[derive(Debug, Fail)]
pub enum FeaturesError {
    /// Feature does not contain a feature/value separator.
    #[fail(
        display = "feature does not contain a feature/value separator: {}",
        value
    )]
    NoFeatureValueSeparator { value: String },
}

/// Token features.
///
/// In the CoNLL-X specification, these are morphological features of the
/// token. Typically, the features are a list or a key-value mapping.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Features {
    inner: BTreeMap<String, String>,
}

impl Features {
    /// Construct an empty set of features.
    pub fn new() -> Self {
        Features {
            inner: BTreeMap::new(),
        }
    }

    /// Unwrap the contained feature map.
    pub fn into_inner(self) -> BTreeMap<String, String> {
        self.inner
    }

    fn parse_features(
        feature_string: impl AsRef<str>,
    ) -> Result<BTreeMap<String, String>, FeaturesError> {
        let mut features = BTreeMap::new();

        for fv in feature_string.as_ref().split('|') {
            let fv: &str = fv;
            let idx = fv.find('=').ok_or(FeaturesError::NoFeatureValueSeparator {
                value: fv.to_string(),
            })?;

            let k = fv[..idx].to_owned();
            let v = fv[idx + 1..].to_owned();

            features.insert(k, v);
        }

        Ok(features)
    }
}

impl Default for Features {
    fn default() -> Self {
        Features::new()
    }
}

impl Deref for Features {
    type Target = BTreeMap<String, String>;

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

impl From<BTreeMap<String, String>> for Features {
    fn from(feature_map: BTreeMap<String, String>) -> Self {
        Features { inner: feature_map }
    }
}

impl TryFrom<&str> for Features {
    type Error = FeaturesError;

    fn try_from(feature_string: &str) -> Result<Self, Self::Error> {
        Ok(Features {
            inner: Features::parse_features(feature_string)?,
        })
    }
}

impl<S, T> FromIterator<(S, T)> for Features
where
    S: Into<String>,
    T: Into<String>,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (S, T)>,
    {
        let features = BTreeMap::from_iter(iter.into_iter().map(|(k, v)| (k.into(), v.into())));

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
            .map(|(k, v)| format!("{}={}", k, v))
            .join("|")
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::convert::TryFrom;
    use std::iter::FromIterator;

    use maplit::btreemap;
    use quickcheck::quickcheck;

    use super::{Features, Token, TokenBuilder};

    quickcheck! {
        fn features_from_iter(feature_map: BTreeMap<String, String>) -> bool{
            feature_map == *Features::from_iter(feature_map.clone())
        }
    }

    #[test]
    fn features_from_iter_as_string() {
        let feature_map = btreemap! {
            "feature2" => "y",
            "feature1" => "x"
        };

        let features = Features::from_iter(feature_map);
        let features_string: String = features.into();

        assert_eq!(features_string, "feature1=x|feature2=y");
    }

    #[test]
    fn features_with_equals() {
        let f = "Some=feature=with|additional=colons";
        let features = Features::try_from(f).unwrap();
        let some = features.get("Some").map(String::as_str);
        assert_eq!(some, Some("feature=with"));
        let additional = features.get("additional").map(String::as_str);
        assert_eq!(additional, Some("colons"));
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
                .upos("N")
                .xpos("NE")
                .features(
                    Features::try_from("case=nominative|number=singular|gender=masculine").unwrap(),
                )
                .into(),
            TokenBuilder::new("Deleuze")
                .lemma("Deleuze")
                .upos("N")
                .xpos("NE")
                .features(
                    Features::try_from("case=nominative|number=singular|gender=masculine").unwrap(),
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

    #[test]
    fn eq_features_is_order_insensitive() {
        let token1: Token = TokenBuilder::new("a")
            .features(Features::try_from("a=d|b=c").unwrap())
            .into();
        let token2 = TokenBuilder::new("a")
            .features(Features::try_from("b=c|a=d").unwrap())
            .into();

        assert_eq!(token1, token2);
    }

    #[test]
    #[should_panic]
    fn parsing_features_fails_with_missing_separatpr() {
        Features::try_from("a|b").unwrap();
    }
}
