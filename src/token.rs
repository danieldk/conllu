//! Tokens in the dependency graph.

use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::Display;
use std::iter::FromIterator;
use std::mem;
use std::ops::{Deref, DerefMut};

use itertools::Itertools;

use crate::error::ParseError;

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
        self.token.set_features(features);
        self
    }

    #[allow(dead_code)]
    pub(crate) fn deps(mut self, deps: impl Into<String>) -> TokenBuilder {
        self.token.set_deps(Some(deps.into()));
        self
    }

    /// Set miscellaneous token features.
    pub fn misc(mut self, misc: Misc) -> TokenBuilder {
        self.token.set_misc(misc);
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
    features: Features,
    misc: Misc,

    // Currently not exposed, but stored to preserve existing
    // field on read -> write round trips.
    deps: Option<String>,
}

impl Token {
    /// Create a new token where all the non-form fields are absent.
    pub fn new(form: impl Into<String>) -> Token {
        Token {
            form: form.into(),
            lemma: None,
            upos: None,
            xpos: None,
            features: Features::new(),
            misc: Misc::new(),
            deps: None,
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

    /// Get the universal part-of-speech tag.
    pub fn upos(&self) -> Option<&str> {
        self.upos.as_ref().map(String::as_ref)
    }

    /// Get the language-specific part-of-speech tag.
    pub fn xpos(&self) -> Option<&str> {
        self.xpos.as_ref().map(String::as_ref)
    }

    /// Get the syntactic and/or morphological features of the token.
    pub fn features(&self) -> &Features {
        &self.features
    }

    /// Get the syntactic and/or morphological features of the token.
    ///
    /// Returns a mutable reference, so that the features can be updated.
    pub fn features_mut(&mut self) -> &mut Features {
        &mut self.features
    }

    pub(crate) fn deps(&self) -> Option<&str> {
        self.deps.as_deref()
    }

    /// Get miscellaneous token features.
    pub fn misc(&self) -> &Misc {
        &self.misc
    }

    /// Get miscellaneous token features.
    ///
    /// Returns a mutable reference, so that the information can be updated.
    pub fn misc_mut(&mut self) -> &mut Misc {
        &mut self.misc
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

    /// Set the universal part-of-speech tag.
    ///
    /// Returns the universal part-of-speech tag that is replaced.
    pub fn set_upos<S>(&mut self, upos: Option<S>) -> Option<String>
    where
        S: Into<String>,
    {
        mem::replace(&mut self.upos, upos.map(Into::into))
    }

    /// Set the language-specific part-of-speech tag.
    ///
    /// Returns the language-specific part-of-speech tag that is replaced.
    pub fn set_xpos<S>(&mut self, xpos: Option<S>) -> Option<String>
    where
        S: Into<String>,
    {
        mem::replace(&mut self.xpos, xpos.map(Into::into))
    }

    /// Set the syntactic and/or morphological features of the token.
    ///
    /// Returns the features that are replaced.
    pub fn set_features(&mut self, features: Features) -> Features {
        mem::replace(&mut self.features, features)
    }

    pub(crate) fn set_deps(&mut self, deps: Option<impl Into<String>>) -> Option<String> {
        mem::replace(&mut self.deps, deps.map(Into::into))
    }

    /// Set miscellaneous token features.
    ///
    /// Returns the features that are replaced.
    pub fn set_misc(&mut self, misc: Misc) -> Misc {
        mem::replace(&mut self.misc, misc)
    }
}

/// Token features.
///
/// In the CoNLL-U specification, these are morphological features of the
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

    fn parse_features(feature_string: impl AsRef<str>) -> Result<Self, ParseError> {
        let mut features = BTreeMap::new();

        if feature_string.as_ref() == "_" {
            return Ok(Features {
                inner: BTreeMap::new(),
            });
        }

        for fv in feature_string.as_ref().split('|') {
            let idx = fv.find('=').ok_or(ParseError::IncorrectFeatureField {
                value: fv.to_owned(),
            })?;

            features.insert(fv[..idx].to_owned(), fv[idx + 1..].to_owned());
        }

        Ok(Features { inner: features })
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
    type Error = ParseError;

    fn try_from(feature_string: &str) -> Result<Self, Self::Error> {
        Features::parse_features(feature_string)
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
        if features.is_empty() {
            "_".to_string()
        } else {
            features
                .inner
                .iter()
                .map(|(k, v)| format!("{}={}", k, v))
                .join("|")
        }
    }
}

/// Miscellaneous features.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Misc {
    inner: BTreeMap<String, Option<String>>,
}

impl Misc {
    /// Construct an empty set of features.
    pub fn new() -> Self {
        Misc {
            inner: BTreeMap::new(),
        }
    }

    /// Unwrap the contained feature map.
    pub fn into_inner(self) -> BTreeMap<String, Option<String>> {
        self.inner
    }

    fn parse_features(misc_string: impl AsRef<str>) -> BTreeMap<String, Option<String>> {
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

impl Default for Misc {
    fn default() -> Self {
        Misc::new()
    }
}

impl Deref for Misc {
    type Target = BTreeMap<String, Option<String>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Misc {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Display for Misc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let misc_str: String = self.into();
        f.write_str(&misc_str)
    }
}

impl From<BTreeMap<String, Option<String>>> for Misc {
    fn from(misc_map: BTreeMap<String, Option<String>>) -> Self {
        Misc { inner: misc_map }
    }
}

impl From<&str> for Misc {
    fn from(misc_string: &str) -> Self {
        Misc {
            inner: Misc::parse_features(misc_string),
        }
    }
}

impl<S, T> FromIterator<(S, Option<T>)> for Misc
where
    S: Into<String>,
    T: Into<String>,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (S, Option<T>)>,
    {
        let misc =
            BTreeMap::from_iter(iter.into_iter().map(|(k, v)| (k.into(), v.map(Into::into))));

        Misc { inner: misc }
    }
}

impl From<Misc> for String {
    fn from(misc: Misc) -> Self {
        (&misc).into()
    }
}

impl From<&Misc> for String {
    fn from(misc: &Misc) -> Self {
        if misc.is_empty() {
            "_".to_string()
        } else {
            misc.inner
                .iter()
                .map(|(k, v)| match *v {
                    Some(ref v) => format!("{}={}", k, v),
                    None => k.to_owned(),
                })
                .join("|")
        }
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
    use crate::error::ParseError;

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

        assert_eq!(String::from(Features::new()), "_");
    }

    #[test]
    fn features_with_colons() {
        let f = "Some=feature=with|additional=colons";
        let features = Features::try_from(f).unwrap();
        let some = features.get("Some").unwrap();
        assert_eq!(some, "feature=with");
        let additional = features.get("additional").unwrap();
        assert_eq!(additional, "colons");
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
            .features(Features::try_from("a=b|c=d").unwrap())
            .into();
        let token2 = TokenBuilder::new("a")
            .features(Features::try_from("c=d|a=b").unwrap())
            .into();

        assert_eq!(token1, token2);
    }

    #[test]
    fn feature_without_value_results_in_error() {
        assert_eq!(
            Features::try_from("c=d|a"),
            Err(ParseError::IncorrectFeatureField {
                value: "a".to_string()
            })
        );
    }

    #[test]
    fn parse_empty_features() {
        assert_eq!(Features::try_from("_").unwrap(), Features::new());
    }
}
