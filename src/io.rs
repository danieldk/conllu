//! CoNLL-U format reader and writers.

use std::convert::TryFrom;
use std::io;

use failure::Error;

use crate::error::ReadError;
use crate::graph::{DepTriple, Sentence};
use crate::token::{Features, Token, EMPTY_TOKEN};

/// A trait for objects that can read CoNLL-U `Sentence`s
pub trait ReadSentence {
    /// Read a `Sentence` from this object.
    ///
    /// # Errors
    ///
    /// A call to `read_sentence` may generate an error to indicate that
    /// the operation could not be completed.
    fn read_sentence(&mut self) -> Result<Option<Sentence>, Error>;

    /// Get an iterator over the sentences in this reader.
    fn sentences(self) -> Sentences<Self>
    where
        Self: Sized,
    {
        Sentences { reader: self }
    }
}

/// A reader for CoNLL-U sentences.
pub struct Reader<R> {
    read: R,
}

impl<R: io::BufRead> Reader<R> {
    /// Construct a new reader from an object that implements the
    /// `io::BufRead` trait.
    pub fn new(read: R) -> Reader<R> {
        Reader { read }
    }
}

impl<R: io::BufRead> IntoIterator for Reader<R> {
    type Item = Result<Sentence, Error>;
    type IntoIter = Sentences<Reader<R>>;

    fn into_iter(self) -> Self::IntoIter {
        self.sentences()
    }
}

impl<R: io::BufRead> ReadSentence for Reader<R> {
    fn read_sentence(&mut self) -> Result<Option<Sentence>, Error> {
        let mut line = String::new();
        let mut sentence = Sentence::new();
        let mut edges = Vec::new();
        let mut proj_edges = Vec::new();

        loop {
            line.clear();

            // End of reader.
            if self.read.read_line(&mut line)? == 0 {
                if sentence.len() == 1 {
                    return Ok(None);
                }

                add_edges(&mut sentence, edges, proj_edges);

                return Ok(Some(sentence));
            }

            // The blank line is a sentence separator. We want to be robust
            // in the case a CoNLL file is malformed and has two newlines as
            // a separator.
            if line.trim().is_empty() {
                if sentence.len() == 1 {
                    continue;
                }

                add_edges(&mut sentence, edges, proj_edges);

                return Ok(Some(sentence));
            }

            let mut iter = line.trim().split_terminator('\t');

            parse_identifier_field(iter.next())?;

            let mut token = Token::new(parse_form_field(iter.next())?);
            token.set_lemma(parse_string_field(iter.next()));
            token.set_upos(parse_string_field(iter.next()));
            token.set_xpos(parse_string_field(iter.next()));
            token.set_features(
                parse_string_field(iter.next())
                    .map(|s| Features::try_from(s.as_str()))
                    .transpose()?,
            );

            // Head relation.
            if let Some(head) = parse_numeric_field(iter.next())? {
                let head_rel = parse_string_field(iter.next());
                edges.push(DepTriple::new(head, head_rel, sentence.len()));
            }

            // Projective head relation.
            if let Some(proj_head) = parse_numeric_field(iter.next())? {
                let proj_head_rel = parse_string_field(iter.next());
                proj_edges.push(DepTriple::new(proj_head, proj_head_rel, sentence.len()));
            }

            //token.set_head();
            //token.set_head_rel();
            //token.set_p_head(parse_numeric_field(iter.next())?);
            //token.set_p_head_rel(parse_string_field(iter.next()));

            sentence.push(token);
        }
    }
}

fn add_edges(
    sentence: &mut Sentence,
    edges: Vec<DepTriple<String>>,
    proj_edges: Vec<DepTriple<String>>,
) {
    for edge in edges {
        sentence.dep_graph_mut().add_deprel(edge);
    }

    for edge in proj_edges {
        sentence.proj_dep_graph_mut().add_deprel(edge);
    }
}

/// An iterator over the sentences in a `Reader`.
pub struct Sentences<R>
where
    R: ReadSentence,
{
    reader: R,
}

impl<R> Iterator for Sentences<R>
where
    R: ReadSentence,
{
    type Item = Result<Sentence, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.reader.read_sentence() {
            Ok(None) => None,
            Ok(Some(sent)) => Some(Ok(sent)),
            Err(e) => Some(Err(e)),
        }
    }
}

fn parse_form_field(field: Option<&str>) -> Result<String, ReadError> {
    field.map(str::to_owned).ok_or(ReadError::MissingFormField)
}

fn parse_string_field(field: Option<&str>) -> Option<String> {
    field.and_then(|s| {
        if s == EMPTY_TOKEN {
            None
        } else {
            Some(s.to_string())
        }
    })
}

fn parse_identifier_field(field: Option<&str>) -> Result<Option<usize>, ReadError> {
    match field {
        None => Err(ReadError::ParseIdentifierField {
            value: "A token identifier should be present".to_owned(),
        }),
        Some(s) => {
            if s == EMPTY_TOKEN {
                return Err(ReadError::ParseIdentifierField {
                    value: s.to_owned(),
                });
            }

            Ok(Some(s.parse::<usize>().map_err(|_| {
                ReadError::ParseIntField {
                    value: s.to_owned(),
                }
            })?))
        }
    }
}

fn parse_numeric_field(field: Option<&str>) -> Result<Option<usize>, ReadError> {
    match field {
        None => Ok(None),
        Some(s) => {
            if s == EMPTY_TOKEN {
                Ok(None)
            } else {
                Ok(Some(s.parse::<usize>().map_err(|_| {
                    ReadError::ParseIntField {
                        value: s.to_owned(),
                    }
                })?))
            }
        }
    }
}

/// A trait for objects that can write CoNLL-U `Sentence`s.
pub trait WriteSentence {
    /// Write a sentence into this object.
    ///
    /// # Errors
    ///
    /// A call to `write_sentence` may generate an error to indicate that
    /// the operation could not be completed.
    fn write_sentence(&mut self, sentence: &Sentence) -> Result<(), Error>;
}

/// A writer for CoNLL-U sentences.
///
/// This writer will write sentences to the embedded writer in CoNLL-U
/// tabular format.
pub struct Writer<W> {
    write: W,
    first: bool,
}

impl<W: io::Write> Writer<W> {
    /// Construct a new writer from an object that implements the `io::Write`
    /// trait.
    pub fn new(write: W) -> Writer<W> {
        Writer { write, first: true }
    }

    /// Borrow the embedded writer. Getting the underlying writer is often
    /// useful when the writer writes to a memory object.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::str;
    ///
    /// use conllu::io::{Writer, WriteSentence};
    /// use conllu::graph::Sentence;
    /// use conllu::token::Token;
    ///
    /// let output = Vec::new();
    /// let mut writer = Writer::new(output);
    /// let mut sent = Sentence::new();
    /// sent.push(Token::new("hello"));
    /// sent.push(Token::new("world"));
    ///
    /// writer.write_sentence(&sent).unwrap();
    ///
    /// println!("Output:\n{}", str::from_utf8(writer.get_ref()).unwrap());
    /// ```
    pub fn get_ref(&self) -> &W {
        &self.write
    }
}

impl<W: io::Write> WriteSentence for Writer<W> {
    fn write_sentence(&mut self, sentence: &Sentence) -> Result<(), Error> {
        if self.first {
            self.first = false;
            write!(self.write, "{}", sentence)?
        } else {
            write!(self.write, "\n{}", sentence)?
        }

        Ok(())
    }
}

/// A writer for CoNLL-U sentences that partitions incoming objects
/// among multiple writers.
///
/// For example, suppose that a `PartitioningWriter` is wraps writers
/// *w1*, *w2*, and sentences *s[1-5]* are written. The sentences are then
/// written as follows:
///
/// * s1 -> w1
/// * s2 -> w2
/// * s3 -> w1
/// * s4 -> w2
/// * s5 -> w1
pub struct PartitioningWriter<W>
where
    W: WriteSentence,
{
    writers: Vec<W>,
    fold: usize,
}

impl<W> PartitioningWriter<W>
where
    W: WriteSentence,
{
    pub fn new(writers: Vec<W>) -> PartitioningWriter<W> {
        PartitioningWriter { writers, fold: 0 }
    }
}

impl<W> WriteSentence for PartitioningWriter<W>
where
    W: WriteSentence,
{
    fn write_sentence(&mut self, sentence: &Sentence) -> Result<(), Error> {
        if self.fold == self.writers.len() {
            self.fold = 0
        }

        self.writers[self.fold].write_sentence(sentence)?;
        self.fold += 1;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::{BufRead, Cursor, Read};
    use std::str;

    use failure::Error;

    use super::{ReadSentence, WriteSentence, Writer};
    use crate::graph::Sentence;
    use crate::tests::{read_sentences, TEST_SENTENCES};

    static BASIC: &str = "testdata/basic.conll";

    static DOUBLE_NEWLINE: &str = "testdata/double-newline.conll";

    static EMPTY: &str = "testdata/empty.conll";

    fn read_file(filename: &str) -> Result<String, Error> {
        let mut f = File::open(filename)?;
        let mut contents = String::new();
        f.read_to_string(&mut contents)?;
        Ok(contents)
    }

    fn string_reader(s: &str) -> Box<dyn BufRead> {
        Box::new(Cursor::new(s.as_bytes().to_owned()))
    }

    fn test_parsing(correct: &[Sentence], fragment: &str) {
        let sentences = read_sentences(fragment);
        assert_eq!(correct.as_ref(), sentences.as_slice());
    }

    #[test]
    fn reader() {
        test_parsing(&*TEST_SENTENCES, BASIC);
    }

    #[test]
    fn reader_robust() {
        test_parsing(&*TEST_SENTENCES, DOUBLE_NEWLINE);
    }

    #[test]
    fn reader_marked_empty() {
        test_parsing(&*TEST_SENTENCES, EMPTY);
    }

    #[test]
    #[should_panic(expected = "ParseIntField")]
    fn reader_rejects_non_numeric_id() {
        let mut reader = super::Reader::new(string_reader("test"));
        reader.read_sentence().unwrap();
    }

    #[test]
    #[should_panic(expected = "ParseIdentifierField")]
    fn reader_rejects_underscore_id() {
        let mut reader = super::Reader::new(string_reader("_"));
        reader.read_sentence().unwrap();
    }

    #[test]
    fn writer() {
        let output = Vec::new();
        let mut writer = Writer::new(Box::new(output));

        for sentence in &*TEST_SENTENCES {
            writer.write_sentence(&sentence).unwrap();
        }

        assert_eq!(
            read_file(EMPTY).unwrap(),
            str::from_utf8(writer.get_ref()).unwrap()
        );
    }
}
