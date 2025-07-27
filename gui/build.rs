use std::io::Result;

fn main() -> Result<()> {
    prost_build::compile_protos(&["../proto/messaging.proto"], &["../proto"])?;
    Ok(())
}
