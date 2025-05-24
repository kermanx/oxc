use std::{
    io::Write,
    process::{Command, Stdio},
};

use crate::logln;

use super::add_header;

/// Format MoonBit code, and add header.
pub fn print_moonbit(code: &str, generator_path: &str) -> String {
    let code = add_header(code, generator_path, "//");
    format(&code)
}

/// Format MoonBit code with `moon fmt`.
fn format(source_text: &str) -> String {
    source_text.to_string()
    // let mut dprint = Command::new("moon")
    //     .stdin(Stdio::piped())
    //     .stdout(Stdio::piped())
    //     .stderr(Stdio::piped())
    //     .args(["fmt"])
    //     .spawn()
    //     .expect("Failed to run moon (is it installed?)");

    // let stdin = dprint.stdin.as_mut().unwrap();
    // stdin.write_all(source_text.as_bytes()).unwrap();
    // stdin.flush().unwrap();

    // let output = dprint.wait_with_output().unwrap();
    // if output.status.success() {
    //     String::from_utf8(output.stdout).unwrap()
    // } else {
    //     // Formatting failed. Return unformatted code, to aid debugging.
    //     let error =
    //         String::from_utf8(output.stderr).unwrap_or_else(|_| "Unknown error".to_string());
    //     logln!("FAILED TO FORMAT JS/TS code:\n{error}");
    //     source_text.to_string()
    // }
}
