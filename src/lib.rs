#![allow(dead_code)]

#[derive(Debug, PartialEq, Clone)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type Input<'a> = &'a str;

type ParsedResult<'a, Output> = Result<(Input<'a>, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: Input<'a>) -> ParsedResult<'a, Output>;

    fn map<F, U>(self, map_fn: F) -> BoxedParser<'a, U>
    where
        Self: Sized + 'a,
        Output: 'a,
        U: 'a,
        F: Fn(Output) -> U + 'a,
    {
        BoxedParser::new(map_parser_res(self, map_fn))
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    fn and_then<F, P, R>(self, then_fn: F) -> BoxedParser<'a, R>
    where
        Self: Sized + 'a,
        R: 'a,
        Output: 'a,
        P: Parser<'a, R> + 'a,
        F: Fn(Output) -> P + 'a,
    {
        BoxedParser::new(and_then(self, then_fn))
    }
}

impl<'a, T, F> Parser<'a, T> for F
where
    F: Fn(Input<'a>) -> ParsedResult<'a, T>,
{
    fn parse(&self, input: Input<'a>) -> ParsedResult<'a, T> {
        self(input)
    }
}

fn the_letter_a<'a>(input: Input<'a>) -> ParsedResult<'a, ()> {
    match input.chars().next() {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err("Not a char a!"),
    }
}

fn match_literal<'a>(expected: &'a str) -> impl Parser<'a, ()> {
    move |input: Input<'a>| {
        let expected_len = expected.len();
        match input.get(0..expected_len) {
            Some(next) if next == expected => Ok((&input[expected_len..], ())),
            _ => Err(input),
        }
    }
}

fn identifier<'a>(input: Input<'a>) -> ParsedResult<'a, String> {
    let mut chars = input.chars();
    let mut matched = String::new();

    match chars.next() {
        Some(first) if first.is_alphabetic() => {
            matched.push(first);
        }
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphabetic() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    Ok((&input[matched.len()..], matched))
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    R1: 'a + Clone,
    R2: 'a,
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(next_input, result2)| (next_input, (result1, result2)))
        })
    }
}

fn map_parser_res<'a, P, R, F, U>(parser: P, map_fn: F) -> impl Parser<'a, U>
where
    P: Parser<'a, R>,
    F: Fn(R) -> U,
{
    move |input: Input<'a>| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    R1: Clone + 'a,
    R2: 'a,
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
{
    map_parser_res(pair(parser1, parser2), |(left, _)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    R1: Clone + 'a,
    R2: 'a,
    P1: Parser<'a, R1> + 'a,
    P2: Parser<'a, R2> + 'a,
{
    pair(parser1, parser2).map(|(_, right)| right)
}

fn one_or_more<'a, P, R>(parser: P) -> impl Parser<'a, Vec<R>>
where
    P: Parser<'a, R>,
{
    move |mut input: Input<'a>| {
        let mut result: Vec<R> = Vec::new();

        if let Ok((next_input, first_res)) = parser.parse(input) {
            result.push(first_res);
            input = next_input;
        } else {
            return Err(input);
        }

        while let Ok((next_input, next_result)) = parser.parse(input) {
            result.push(next_result);
            input = next_input;
        }

        Ok((input, result))
    }
}

fn zero_or_more<'a, P, R>(parser: P) -> impl Parser<'a, Vec<R>>
where
    P: Parser<'a, R>,
{
    move |mut input: Input<'a>| {
        let mut result: Vec<R> = Vec::new();

        while let Ok((next_input, next_result)) = parser.parse(input) {
            result.push(next_result);
            input = next_input;
        }

        Ok((input, result))
    }
}

fn any_char<'a>(input: Input<'a>) -> ParsedResult<'a, char> {
    match input.chars().next() {
        Some(next_char) => Ok((&input[next_char.len_utf8()..], next_char)),
        _ => Err(input),
    }
}

fn pred<'a, P, F, R>(parser: P, predicate: F) -> impl Parser<'a, R>
where
    P: Parser<'a, R>,
    F: Fn(&R) -> bool,
{
    move |input| {
        if let Ok((next_input, result)) = parser.parse(input) {
            if predicate(&result) {
                return Ok((next_input, result));
            }
        }

        Err(input)
    }
}

fn white_space_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(white_space_char())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(white_space_char())
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    )
    .map(|chars| chars.into_iter().collect())
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(match_literal("="), quoted_string()))
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(match_literal("<"), pair(identifier, attributes()))
}

fn single_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal("/>")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

fn open_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal(">")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

fn element<'a>() -> impl Parser<'a, Element> {
    whitespace_wrapper(either(single_element(), parent_element()))
}

fn close_element<'a>(expected_name: String) -> impl Parser<'a, String> {
    right(match_literal("</"), left(identifier, match_literal(">")))
        .pred(move |name| name == &expected_name)
}

fn parent_element<'a>() -> impl Parser<'a, Element> {
    open_element().and_then(|open_elem| {
        left(
            zero_or_more(element()),
            close_element(open_elem.name.clone()),
        )
        .map(move |children| {
            let mut el = open_elem.clone();
            el.children = children;
            el
        })
    })
}

fn either<'a, P1, P2, R>(parser1: P1, parser2: P2) -> impl Parser<'a, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    move |input| parser1.parse(input).or_else(|_| parser2.parse(input))
}

fn and_then<'a, P, F, R1, P2, R2>(p: P, f: F) -> impl Parser<'a, R2>
where
    P: Parser<'a, R1>,
    P2: Parser<'a, R2>,
    F: Fn(R1) -> P2,
{
    move |input| match p.parse(input) {
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}

fn whitespace_wrapper<'a, P, R>(parser: P) -> impl Parser<'a, R>
where
    R: 'a + Clone,
    P: Parser<'a, R> + 'a,
{
    right(space0(), left(parser, space0()))
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: Input<'a>) -> ParsedResult<'a, Output> {
        self.parser.parse(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_the_letter_a() {
        assert_eq!(Ok(("lex", ())), the_letter_a("alex"));
        assert_eq!(Err("Not a char a!"), the_letter_a("lex"));
    }

    #[test]
    fn literal_parser() {
        let parser = match_literal("Hello Joe!");

        assert_eq!(Ok(("", ())), parser.parse("Hello Joe!"));

        assert_eq!(
            Ok((" Hello Robert!", ())),
            parser.parse("Hello Joe! Hello Robert!")
        );

        assert_eq!(Err("Hello Mike!"), parser.parse("Hello Mike!"));
    }

    #[test]
    fn identifier_parser() {
        assert_eq!(
            Ok(("", "i-am-an-identifier".to_string())),
            identifier("i-am-an-identifier")
        );
        assert_eq!(
            Ok((" entirely an identifier", "not".to_string())),
            identifier("not entirely an identifier")
        );
        assert_eq!(
            Err("!not at all an identifier"),
            identifier("!not at all an identifier")
        );
    }

    #[test]
    fn pair_combinator() {
        let tag_opener = pair(match_literal("<"), identifier);
        assert_eq!(
            Ok(("/>", ((), "my-first-element".to_string()))),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
    }

    #[test]
    fn right_combinator() {
        let tag_opener = right(match_literal("<"), identifier);
        assert_eq!(
            Ok(("/>", "my-first-element".to_string())),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
    }

    #[test]
    fn one_or_more_combinator() {
        let parser = one_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Err("ahah"), parser.parse("ahah"));
        assert_eq!(Err(""), parser.parse(""));
    }

    #[test]
    fn zero_or_more_combinator() {
        let parser = zero_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
        assert_eq!(Ok(("", vec![])), parser.parse(""));
    }

    #[test]
    fn predicate_combinator() {
        let parser = pred(any_char, |c| *c == 'o');
        assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
        assert_eq!(Err("lol"), parser.parse("lol"));
    }

    #[test]
    fn quoted_string_parser() {
        assert_eq!(
            Ok(("", "Hello Joe!".to_string())),
            quoted_string().parse("\"Hello Joe!\"")
        );
    }

    #[test]
    fn attribute_parser() {
        assert_eq!(
            Ok((
                "",
                vec![
                    ("one".to_string(), "1".to_string()),
                    ("two".to_string(), "2".to_string())
                ]
            )),
            attributes().parse(" one=\"1\"   two=\"2\"")
        );
    }

    #[test]
    fn single_element_parser() {
        assert_eq!(
            Ok((
                "",
                Element {
                    name: "div".to_string(),
                    attributes: vec![("class".to_string(), "float".to_string())],
                    children: vec![]
                }
            )),
            single_element().parse("<div class=\"float\"/>")
        );
    }

    #[test]
    fn xml_parser() {
        let doc = r#"
        <top label="Top">
            <semi-bottom label="Bottom"/>
            <middle>
                <bottom label="Another bottom"/>
            </middle>
        </top>"#;
        let parsed_doc = Element {
            name: "top".to_string(),
            attributes: vec![("label".to_string(), "Top".to_string())],
            children: vec![
                Element {
                    name: "semi-bottom".to_string(),
                    attributes: vec![("label".to_string(), "Bottom".to_string())],
                    children: vec![],
                },
                Element {
                    name: "middle".to_string(),
                    attributes: vec![],
                    children: vec![Element {
                        name: "bottom".to_string(),
                        attributes: vec![("label".to_string(), "Another bottom".to_string())],
                        children: vec![],
                    }],
                },
            ],
        };
        assert_eq!(Ok(("", parsed_doc)), element().parse(doc));
    }

    #[test]
fn mismatched_closing_tag() {
    let doc = r#"
        <top>
            <bottom/>
        </middle>"#;
    assert_eq!(Err("</middle>"), element().parse(doc));
}
}
