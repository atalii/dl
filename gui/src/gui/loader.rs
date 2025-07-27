use crate::db;
use eframe::{
    self,
    egui::{Color32, FontFamily, FontId, TextFormat, text::LayoutJob},
};
use log::warn;
use std::env;
use std::io;
use std::os::unix::net::UnixStream;
use std::sync::mpsc;
use std::thread;

/// A Loader looks for a database to connect to and gathers information prerequisite to a run.
pub struct Loader {
    state: Status,
    state_rx: mpsc::Receiver<Status>,
    success_notifier: mpsc::Sender<()>,
    conn: Option<db::Conn>,
}

impl Loader {
    /// Create a new loader, spawning a thread to start a connection.
    pub fn new(ctx: egui::Context, success_notifier: mpsc::Sender<()>) -> Self {
        let (state_tx, state_rx) = mpsc::channel();
        thread::spawn(move || connect(state_tx, ctx));
        Self {
            state: Status::Starting,
            state_rx,
            success_notifier,
            conn: None,
        }
    }

    pub fn take_conn(&mut self) -> Option<db::Conn> {
        self.conn.take()
    }
}

enum Status {
    Starting,
    Failed(io::Error),
    Connecting(String),
    Connected(Option<db::Conn>),
}

impl eframe::App for Loader {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        match self.state_rx.try_recv() {
            Ok(Status::Connected(Some(conn))) => {
                self.conn = Some(conn);
                self.state = Status::Connected(None);
            }

            Ok(state) => self.state = state,
            Err(_) => (),
        };

        // TODO: get this from the context's styles
        let mut header_format = TextFormat {
            font_id: FontId::new(24.0, FontFamily::Proportional),
            color: Color32::BLACK,
            line_height: Some(36.0),
            ..Default::default()
        };

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.centered_and_justified(|ui| {
                let mut job = LayoutJob::default();

                match self.state {
                    Status::Starting => {
                        job.append("Starting...", 0.0, header_format);
                    }
                    Status::Connecting(ref to) => {
                        job.append("Connecting...\n", 0.0, header_format);

                        let msg = format!("Trying: {to}");
                        job.append(&msg, 0.0, Default::default());
                    }
                    Status::Failed(ref err) => {
                        job.append("Failed to connect to the server.", 0.0, header_format);

                        let msg = format!("\n{err}.");
                        job.append(
                            &msg,
                            0.0,
                            TextFormat {
                                color: Color32::DARK_RED, // TODO: use theme
                                ..Default::default()
                            },
                        );
                    }
                    Status::Connected(_) => {
                        header_format.color = Color32::DARK_GREEN; // TODO: use theme
                        job.append("Success!", 0.0, header_format);
                        self.success_notifier.send(()).unwrap(); // TODO: don't unwrap
                    }
                };

                ui.label(job);
            });
        });
    }
}

fn connect(state_tx: mpsc::Sender<Status>, ctx: egui::Context) {
    let send = |status| {
        if let Err(e) = state_tx.send(status) {
            warn!("Couldn't inform GUI of connection status: {e}");
        };
        ctx.request_repaint();
    };

    let path = env::var("XDG_RUNTIME_DIR").unwrap_or_else(|err| {
        warn!("Failed to read XDG_RUNTIME_DIR: {err}");
        "/tmp".to_string()
    }) + "/datalog.sock";

    send(Status::Connecting(path.clone()));

    let sock = match UnixStream::connect(path) {
        Ok(sock) => sock,
        Err(e) => {
            send(Status::Failed(e));
            return;
        }
    };

    let conn = db::Conn::begin(sock);
    send(Status::Connected(Some(conn)));
}
