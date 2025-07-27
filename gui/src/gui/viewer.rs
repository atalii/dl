use crate::db;
use rfd::FileDialog;
use std::path::PathBuf;

use eframe::{
    self,
    egui::{self, Button, RichText, Widget},
};

use log::{error, info};

/// A Viewer allows for interaction with a number of Datalog databases.
pub struct Viewer {
    file_rx: std::sync::mpsc::Receiver<PathBuf>,
    data: std::sync::Arc<ViewerData>,
    _conn: db::Conn,
}

struct ViewerData {
    file_tx: std::sync::mpsc::Sender<PathBuf>,
}

impl Viewer {
    pub fn new(conn: db::Conn) -> Self {
        let (file_tx, file_rx) = std::sync::mpsc::channel();
        Self {
            file_rx,
            data: ViewerData { file_tx }.into(),
            _conn: conn,
        }
    }
}

impl eframe::App for Viewer {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if let Ok(path) = self.file_rx.try_recv() {
            info!("Selected: {path:?}");
        }

        egui::CentralPanel::default().show(ctx, |ui| {
            egui::Grid::new("main_grid").show(ui, |ui| {
                let open_file_btn = ui.vertical_centered(|ui| {
                    Button::new(RichText::new("Open file.").size(14.0))
                        .min_size((144., 72.).into())
                        .ui(ui)
                });

                if open_file_btn.inner.clicked() {
                    let data = self.data.clone();
                    std::thread::spawn(move || data.open_file());
                }

                ui.end_row();
            })
        });
    }
}

impl ViewerData {
    fn open_file(&self) {
        match FileDialog::new().pick_file() {
            Some(path) => {
                // TODO: a thread should monitor this channel and request a repaint once
                // something's available.
                if let Err(e) = self.file_tx.send(path) {
                    error!("Can't inform app of selected file: {e:?}");
                }
            }
            None => info!("No file was selected."),
        };
    }
}
