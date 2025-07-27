use dlgui::gui::DatalogApp;
use eframe;
use egui::ThemePreference;
use egui::epaint::text::{FontInsert, InsertFontFamily};
use env_logger;

fn main() {
    env_logger::init();
    launch();
}

fn launch() {
    let mut native_options = eframe::NativeOptions::default();
    native_options.window_builder = Some(Box::new(|vb| vb.with_inner_size((480.0, 720.0))));

    eframe::run_native(
        "Datalog",
        native_options,
        Box::new(|cc| {
            // ThemePreference::System doesn't quite seem to work. No idea why.
            cc.egui_ctx.set_theme(ThemePreference::Light);

            cc.egui_ctx.add_font(FontInsert::new(
                "Adwaita Sans",
                egui::FontData::from_static(include_bytes!(
                    "../static/adwaita-fonts-49.0/sans/AdwaitaSans-Regular.ttf"
                )),
                vec![InsertFontFamily {
                    family: egui::FontFamily::Proportional,
                    priority: egui::epaint::text::FontPriority::Highest,
                }],
            ));

            Ok(Box::new(DatalogApp::new(cc.egui_ctx.clone())))
        }),
    )
    .unwrap();
}
