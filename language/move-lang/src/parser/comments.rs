// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use move_symbol_pool::Symbol;
use std::collections::BTreeMap;

/// Types to represent comments.
pub type CommentMap = BTreeMap<Symbol, MatchedFileCommentMap>;
pub type MatchedFileCommentMap = BTreeMap<u32, String>;
pub type FileCommentMap = BTreeMap<(u32, u32), String>;
