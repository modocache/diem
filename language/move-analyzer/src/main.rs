// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{notification::Notification as _, request::Request as _};
use std::str::FromStr;
use structopt::StructOpt;
use url::Url;

#[derive(StructOpt)]
#[structopt(name = "move-analyzer", about = "A language server for Move")]
struct Options {}

fn main() {
    // For now, move-analyzer only responds to options built-in to structopt,
    // such as `--help` or `--version`.
    Options::from_args();

    // stdio is used to communicate Language Server Protocol requests and responses.
    // stderr is used for logging (and, when Visual Studio Code is used to communicate with this
    // server, it captures this output in a dedicated "output channel").
    let exe = std::env::current_exe()
        .unwrap()
        .to_string_lossy()
        .to_string();
    eprintln!(
        "Starting language server '{}' communicating via stdio...",
        exe
    );

    let (connection, io_threads) = Connection::stdio();
    let capabilities = serde_json::to_value(lsp_types::ServerCapabilities {
        text_document_sync: None,
        selection_range_provider: None,
        hover_provider: None,
        completion_provider: None,
        signature_help_provider: None,
        definition_provider: Some(lsp_types::OneOf::Left(true)),
        type_definition_provider: None,
        implementation_provider: None,
        references_provider: None,
        document_highlight_provider: None,
        document_symbol_provider: None,
        workspace_symbol_provider: None,
        code_action_provider: None,
        code_lens_provider: None,
        document_formatting_provider: None,
        document_range_formatting_provider: None,
        document_on_type_formatting_provider: None,
        rename_provider: None,
        document_link_provider: None,
        color_provider: None,
        folding_range_provider: None,
        declaration_provider: None,
        execute_command_provider: None,
        workspace: None,
        call_hierarchy_provider: None,
        semantic_tokens_provider: None,
        moniker_provider: None,
        linked_editing_range_provider: None,
        experimental: None,
    })
    .expect("could not serialize server capabilities");
    connection
        .initialize(capabilities)
        .expect("could not initialize the connection");

    loop {
        if !match connection.receiver.recv() {
            Ok(message) => match message {
                Message::Request(request) => on_request(&connection, &request),
                Message::Response(response) => on_response(&connection, &response),
                Message::Notification(notification) => on_notification(&connection, &notification),
            },
            Err(error) => {
                eprintln!("error: {}", error.to_string());
                false
            }
        } {
            break;
        }
    }

    io_threads.join().expect("I/O thread could not finish");
    eprintln!("Shut down language server '{}'.", exe);
}

fn on_request(connection: &Connection, request: &Request) -> bool {
    if request.method == lsp_types::request::GotoDefinition::METHOD {
        on_go_to_definition_request(connection, request)
    } else {
        todo!("support for this request is not yet implemented (see trace for more details)");
    }
}

fn on_go_to_definition_request(connection: &Connection, request: &Request) -> bool {
    let parameters =
        serde_json::from_value::<lsp_types::GotoDefinitionParams>(request.params.clone())
            .expect("could not deserialize request");
    eprintln!("{:?}", parameters);
    let location = lsp_types::Location::new(
        Url::from_str("file:///home/modocache/src/diem/language/move-analyzer/editors/code/tests/colorize-fixtures/abilities.move").expect("could not parse location url"),
        lsp_types::Range {
            // Note that, unlike how editors typically refer to these indices, LSP expects these to
            // be zero-indexed. So, to refer to "line 3, column 5" in Vim or VS Code, we would need
            // to send a response of `2` and `4`.
            start: lsp_types::Position { line: 2, character: 4 },
            end: lsp_types::Position { line: 2, character: 7 },
        },
    );
    let result = serde_json::to_value(location).expect("could not serialize response");
    let response = lsp_server::Response::new_ok(request.id.clone(), result);
    connection.sender.send(lsp_server::Message::Response(response)).expect("could not send response");
    true
}

fn on_response(_connection: &Connection, response: &Response) -> bool {
    eprintln!("response: {:?}", response);
    todo!("handle response from client");
}

fn on_notification(_connection: &Connection, notification: &Notification) -> bool {
    if notification.method == lsp_types::notification::Exit::METHOD {
        false
    } else {
        todo!("handle notification from client")
    }
}
