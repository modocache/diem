// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

//! The code points enumerated in this file use
//! https://www.unicode.org/Public/UCD/latest/ucd/PropList.txt as a reference.

/// Returns `true` if a character is horizontal whitespace.
pub fn is_unicode_horizontal_whitespace(c: char) -> bool {
    matches!(
        c as u32,
        0x0009 | 0x0020 | 0x00A0 | 0x1680 | 0x2000..=0x200A | 0x202F | 0x205F | 0x3000
    )
}

/// Returns `true` if a character is veritcal whitespace, i.e.: something that a
/// text editor may render as a line-break.
pub fn is_unicode_vertical_whitespace(c: char) -> bool {
    matches!(c as u32, 0x000A..=0x000D | 0x0085 | 0x2028..=0x2029)
}

/// Returns `true` if a character is horizontal or vertical whitespace.
pub fn is_unicode_whitespace(c: char) -> bool {
    is_unicode_horizontal_whitespace(c) || is_unicode_vertical_whitespace(c)
}

/// Returns `true` if a character is a typical ASCII horizontal whitespace.
///
/// Use this function to determine whether a character is a typical horizontal
/// whitespace, as opposed to an esoteric Unicode whitespace character such as
/// `U+2009 THIN SPACE`, which many text editors may choose not to render at
/// all. This function only recognizes the most common kinds of horizontal
/// whitespace: `U+0020 SPACE`, and the horiztonal tab character `U+0009`
/// (which many programming languages represent with the escape sequence `\t`).
pub fn is_permitted_horizontal_whitespace(c: char) -> bool {
    matches!(c as u32, 0x0009 | 0x0020)
}

/// Returns `true` if a character is a typical ASCII newline character.
///
/// Use this function to determine whether a character is a typical line break
/// character , as opposed to an esoteric Unicode vertical whitespace character
/// such as `U+2028 LINE SEPARATOR`, which many text editors may choose not to
/// render at all.
///
/// This function only recognizes the most common vertical whitespace character:
/// whitespace: `U+000A LINE FEED` (which many programming languages represent
/// with the escape sequence `\n`). Notably, it does not recognize
/// `U+000D CARRIAGE RETURN` (typically escaped with the sequence `\r`).
pub fn is_permitted_vertical_whitespace(c: char) -> bool {
    matches!(c as u32, 0x000A)
}

/// Returns `true` if a character is a typical ASCII vertical or horiztonal
/// whitespace.
pub fn is_permitted_whitespace(c: char) -> bool {
    is_permitted_horizontal_whitespace(c) || is_permitted_vertical_whitespace(c)
}

/// Returns `true` if a character is a typical ASCII character that is permitted
/// to appear in Move source code and documentation comments.
///
/// Most Move tools that operate on user-provided text only allow Unicode in
/// (non-documentation) comments. Everywhere else, only these characters, mostly
/// from the ASCII character set, are permitted.
pub fn is_permitted(c: char) -> bool {
    matches!(c as u32, 0x009 | 0x0020..=0x007E) || is_permitted_whitespace(c)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_permitted_characters() {
        let mut good_chars = (0x20..=0x7E).collect::<Vec<u8>>();
        good_chars.push(0x0A); // \n
        good_chars.push(0x09); // \t
        for c in good_chars {
            assert!(super::is_permitted(c as char));
        }
    }

    #[test]
    fn test_forbidden_characters() {
        let mut bad_chars = (0x0..0x09).collect::<Vec<u8>>();
        bad_chars.append(&mut (0x0B..=0x1F).collect::<Vec<u8>>());
        bad_chars.push(0x7F);
        for c in bad_chars {
            assert!(!super::is_permitted(c as char));
        }
    }
}
