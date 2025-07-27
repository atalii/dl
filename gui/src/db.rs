use prost::Message;
use std::io::Read;
use std::io::Write;
use std::os::unix::net::UnixStream;

// Just public so that `cargo doc` works as expected.
pub mod messages {
    include!(concat!(env!("OUT_DIR"), "/datalog.rs"));
}

use messages::query;

pub struct Conn {
    stream: UnixStream,
}

impl Conn {
    pub fn begin(stream: UnixStream) -> Self {
        // this'll be fallible at some point.
        let mut s = Self { stream };
        s.syn();
        s
    }

    fn syn(&mut self) {
        self.write_query(messages::Query {
            content: Some(query::Content::Syn(query::Syn {
                version: "0.1.0".to_string(),
            })),
        });

        let r = self.read_response().content.unwrap(); // TODO
        match r {
            messages::response::Content::Synack(messages::response::SynAck { version })
                if version == "0.1.0" =>
            {
                ()
            }
            messages::response::Content::Synack(_) => panic!("bad server lol"),
        }
    }

    // fn query(&mut self) {}

    fn write_query(&mut self, query: messages::Query) {
        let mut buf = Vec::new();
        let qlen = (query.encoded_len() as u16).to_be_bytes();
        query.encode(&mut buf).unwrap(); // TODO
        self.stream.write_all(&qlen).unwrap(); // TODO
        self.stream.write_all(&buf).unwrap(); // TODO
    }

    fn read_response(&mut self) -> messages::Response {
        let mut buf = [0, 0];
        self.stream.read_exact(&mut buf[..]).unwrap(); // TODO
        let len = u16::from_be_bytes(buf);
        let mut buf = [0].repeat(len.into());
        self.stream.read_exact(buf.as_mut_slice()).unwrap(); // TODO
        messages::Response::decode(buf.as_slice()).unwrap()
    }
}
