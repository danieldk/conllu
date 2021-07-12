//! Tokens in the dependency graph.

use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::mem;
use std::ops::{Deref, DerefMut};

use crate::graph::{Iter, IterMut, Node, Sentence};

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

    /// Set UD enhanced dependencies.
    ///
    /// **Warning:** this method will be removed once proper support for enhanced
    /// dependencies is added.
    pub fn deps(mut self, deps: impl Into<String>) -> TokenBuilder {
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

    /// Get enhanced dependencies.
    ///
    /// **Warning:** this method will be removed once proper support for enhanced
    /// dependencies is added.
    pub fn deps(&self) -> Option<&str> {
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

    /// Set UD enhanced dependencies.
    ///
    /// **Warning:** this method will be removed once proper support for enhanced
    /// dependencies is added.
    pub fn set_deps(&mut self, deps: Option<impl Into<String>>) -> Option<String> {
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

impl From<BTreeMap<String, String>> for Features {
    fn from(feature_map: BTreeMap<String, String>) -> Self {
        Features { inner: feature_map }
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
        let features = iter
            .into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect();

        Features { inner: features }
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

impl From<BTreeMap<String, Option<String>>> for Misc {
    fn from(misc_map: BTreeMap<String, Option<String>>) -> Self {
        Misc { inner: misc_map }
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
        let misc = iter
            .into_iter()
            .map(|(k, v)| (k.into(), v.map(Into::into)))
            .collect();

        Misc { inner: misc }
    }
}

/// Get tokens of a sentence.
pub trait Tokens {
    /// Get an iterator over the tokens in a sentence.
    fn tokens(&self) -> TokenIter;

    /// Get the tokens in a sentence mutably.
    fn tokens_mut(&mut self) -> TokenIterMut;
}

impl Tokens for Sentence {
    fn tokens(&self) -> TokenIter {
        TokenIter { inner: self.iter() }
    }

    fn tokens_mut(&mut self) -> TokenIterMut {
        TokenIterMut {
            inner: self.iter_mut(),
        }
    }
}

/// Token iterator.
pub struct TokenIter<'a> {
    inner: Iter<'a>,
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(node) = self.inner.next() {
            if let Node::Token(token) = node {
                return Some(token);
            }
        }

        None
    }
}

/// Mutable token iterator.
pub struct TokenIterMut<'a> {
    inner: IterMut<'a>,
}

impl<'a> Iterator for TokenIterMut<'a> {
    type Item = &'a mut Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(node) = self.inner.next() {
            if let Node::Token(token) = node {
                return Some(token);
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use std::iter::FromIterator;

    use maplit::btreemap;

    use super::{Features, Tokens};
    use crate::tests::TEST_SENTENCES;

    #[test]
    fn features_from_iter() {
        let feature_map = btreemap! {
            "feature2".to_string() => "y".to_string(),
            "feature1".to_string() => "x".to_string(),
        };

        assert_eq!(feature_map, *Features::from_iter(feature_map.clone()));
    }

    #[test]
    fn tokens() {
        let mut iter = TEST_SENTENCES[0].tokens();
        assert_eq!(iter.next(), TEST_SENTENCES[0][1].token());
        assert_eq!(iter.next(), TEST_SENTENCES[0][2].token());
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn tokens_mut() {
        let mut sentence = TEST_SENTENCES[0].clone();

        {
            let mut iter = sentence.tokens_mut();
            let token = iter.next().unwrap();
            assert_eq!(&*token, TEST_SENTENCES[0][1].token().unwrap());
            token.set_upos(Some("mutable"));
            assert_eq!(iter.next().map(|t| &*t), TEST_SENTENCES[0][2].token());
            assert_eq!(iter.next(), None);
        }

        assert_eq!(sentence[1].token().unwrap().upos(), Some("mutable"));
    }
}
