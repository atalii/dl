use log::warn;
use std::sync::mpsc;
use std::thread;

mod loader;
mod viewer;

pub enum DatalogApp {
    Loading(loader::Loader),
    Viewing(viewer::Viewer),
}

impl DatalogApp {
    pub fn new(ctx: egui::Context) -> Self {
        let (load_tx, load_rx) = mpsc::channel();
        let l = loader::Loader::new(ctx.clone(), load_tx);

        // Launch a thread to be informed when loading finishes.
        thread::spawn(move || {
            match load_rx.recv() {
                Ok(_) => ctx.request_repaint(),
                Err(e) => warn!("Loader notification failed: {e}"),
            };
        });

        Self::Loading(l)
    }
}

impl eframe::App for DatalogApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        use DatalogApp::*;

        match self {
            Viewing(viewer) => viewer.update(ctx, frame),
            Loading(loader) => {
                loader.update(ctx, frame);
                if let Some(conn) = loader.take_conn() {
                    *self = Viewing(viewer::Viewer::new(conn));
                }
            }
        }
    }
}
