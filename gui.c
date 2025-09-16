#include <gtk/gtk.h>
#include <string.h>
#include <stdlib.h>
#include <locale.h>

/* === declaraciones de flex necesarias para usar yy_scan_string === */
typedef struct yy_buffer_state *YY_BUFFER_STATE;
int yyparse(void);
int yylex(void);
YY_BUFFER_STATE yy_scan_string (const char *yy_str);
void yy_delete_buffer (YY_BUFFER_STATE b);

/* hooks exportados por parser.y */
void parser_reset(void);
const char* parser_get_log(void);
const char* exec_get_output(void);
int parser_get_error_count(void);

/* widgets y estado global */
static GtkWidget *txt_input;
static GtkWidget *txt_output;
static GtkWidget *main_win;
static gchar *current_path = NULL;  /* ruta del archivo abierto/guardado */

/* ===== utilidades ===== */
static void set_window_title(const char *filepath) {
  if (filepath && *filepath) {
    gchar *base = g_path_get_basename(filepath);
    gchar *title = g_strdup_printf("USL Parser+Runner — %s", base);
    gtk_window_set_title(GTK_WINDOW(main_win), title);
    g_free(base);
    g_free(title);
  } else {
    gtk_window_set_title(GTK_WINDOW(main_win), "USL Parser+Runner");
  }
}

static void set_output(const char* text){
  GtkTextBuffer *buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(txt_output));
  gtk_text_buffer_set_text(buf, text ? text : "", -1);
}

static gchar* get_input_text(void){
  GtkTextBuffer *buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(txt_input));
  GtkTextIter start, end;
  gtk_text_buffer_get_start_iter(buf, &start);
  gtk_text_buffer_get_end_iter(buf, &end);
  return gtk_text_buffer_get_text(buf, &start, &end, FALSE);
}

/* ===== Analizar & Ejecutar ===== */
static void on_analyze(GtkButton *btn, gpointer user_data){
  (void)btn; (void)user_data;
  gchar *src = get_input_text();
  setlocale(LC_NUMERIC, "C");
  parser_reset(); /* limpia tabla de símbolos, AST y logs */

  YY_BUFFER_STATE buf = yy_scan_string(src ? src : "");
  yyparse();
  yy_delete_buffer(buf);

  const char* log = parser_get_log();
  const char* out = exec_get_output();

  /* Mostrar salida de ejecución si no hubo errores; si hubo, mostrar log */
  GString* final = g_string_new("");
  if (parser_get_error_count()==0) {
    if (out && *out) {
      g_string_append(final, "=== OUTPUT ===\n");
      g_string_append(final, out);
      g_string_append(final, "\n");
    }
  }
  if (log && *log) {
    g_string_append(final, "=== LOG ===\n");
    g_string_append(final, log);
  }

  set_output(final->str);
  g_string_free(final, TRUE);
  g_free(src);
}

/* ===== Limpiar ===== */
static void on_clear(GtkButton *btn, gpointer user_data){
  (void)btn; (void)user_data;
  GtkTextBuffer *ib = gtk_text_view_get_buffer(GTK_TEXT_VIEW(txt_input));
  gtk_text_buffer_set_text(ib, "", -1);
  set_output("");
}

/* ===== Guardar (y Guardar como…) ===== */
static gboolean save_to_path(const gchar *path) {
  gchar *text = get_input_text();
  GError *err = NULL;
  gboolean ok = g_file_set_contents(path, text ? text : "", -1, &err);
  if (!ok) {
    gchar *emsg = g_strdup_printf("No se pudo guardar el archivo:\n%s\n\nError: %s",
                                  path, err ? err->message : "desconocido");
    set_output(emsg);
    g_clear_error(&err);
    g_free(emsg);
  } else {
    GString *msg = g_string_new(">> Guardado correctamente en:\n");
    g_string_append_printf(msg, "%s\n", path);
    set_output(msg->str);
    g_string_free(msg, TRUE);
  }
  g_free(text);
  return ok;
}

static void do_save(GtkWidget *parent) {
  if (current_path && *current_path) {
    /* Guardar directo */
    if (save_to_path(current_path)) set_window_title(current_path);
    return;
  }

  /* Guardar como… (primera vez) */
  GtkWidget *dlg = gtk_file_chooser_dialog_new(
      "Guardar archivo",
      GTK_WINDOW(parent),
      GTK_FILE_CHOOSER_ACTION_SAVE,
      "_Cancelar", GTK_RESPONSE_CANCEL,
      "_Guardar", GTK_RESPONSE_ACCEPT,
      NULL);
  gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dlg), TRUE);

  GtkFileFilter *filter_usl = gtk_file_filter_new();
  gtk_file_filter_set_name(filter_usl, "Archivos USL (*.usl)");
  gtk_file_filter_add_pattern(filter_usl, "*.usl");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dlg), filter_usl);

  if (gtk_dialog_run(GTK_DIALOG(dlg)) == GTK_RESPONSE_ACCEPT) {
    char *path = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dlg));
    if (path) {
      /* asegurar extensión .usl */
      if (!g_str_has_suffix(path, ".usl")) {
        gchar *with_ext = g_strconcat(path, ".usl", NULL);
        g_free(path);
        path = with_ext;
      }
      if (save_to_path(path)) {
        g_clear_pointer(&current_path, g_free);
        current_path = g_strdup(path);
        set_window_title(current_path);
      }
      g_free(path);
    }
  }
  gtk_widget_destroy(dlg);
}

static void on_save(GtkButton *btn, gpointer user_data){
  (void)btn;
  GtkWidget *parent = GTK_WIDGET(user_data);
  do_save(parent);
}

/* ===== Abrir .usl ===== */
static void on_open(GtkButton *btn, gpointer user_data){
  (void)btn;
  GtkWidget *parent = GTK_WIDGET(user_data);

  GtkWidget *dlg = gtk_file_chooser_dialog_new(
      "Abrir archivo .usl",
      GTK_WINDOW(parent),
      GTK_FILE_CHOOSER_ACTION_OPEN,
      "_Cancelar", GTK_RESPONSE_CANCEL,
      "_Abrir", GTK_RESPONSE_ACCEPT,
      NULL);

  GtkFileFilter *filter_usl = gtk_file_filter_new();
  gtk_file_filter_set_name(filter_usl, "Archivos USL (*.usl)");
  gtk_file_filter_add_pattern(filter_usl, "*.usl");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dlg), filter_usl);

  GtkFileFilter *filter_all = gtk_file_filter_new();
  gtk_file_filter_set_name(filter_all, "Todos los archivos");
  gtk_file_filter_add_pattern(filter_all, "*");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dlg), filter_all);

  if (gtk_dialog_run(GTK_DIALOG(dlg)) == GTK_RESPONSE_ACCEPT) {
    char *path = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dlg));
    if (path) {
      gchar *content = NULL; gsize len = 0; GError *err = NULL;
      if (g_file_get_contents(path, &content, &len, &err)) {
        GtkTextBuffer *buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(txt_input));
        gtk_text_buffer_set_text(buf, content, (gint)len);
        g_clear_pointer(&current_path, g_free);
        current_path = g_strdup(path);
        set_window_title(current_path);

        GString *msg = g_string_new(">> Archivo abierto:\n");
        g_string_append_printf(msg, "%s\n", path);
        set_output(msg->str);
        g_string_free(msg, TRUE);
        g_free(content);
      } else {
        gchar *emsg = g_strdup_printf("No se pudo abrir el archivo:\n%s\n\nError: %s",
                                      path, err ? err->message : "desconocido");
        set_output(emsg);
        g_clear_error(&err);
        g_free(emsg);
      }
      g_free(path);
    }
  }

  gtk_widget_destroy(dlg);
}

/* ===== Atajos de teclado (Ctrl+O / Ctrl+S / Ctrl+L) ===== */
static void add_accelerators(GtkWidget *win, GtkWidget *btn_open, GtkWidget *btn_save, GtkWidget *btn_clear) {
  GtkAccelGroup *accel = gtk_accel_group_new();
  gtk_window_add_accel_group(GTK_WINDOW(win), accel);

  gtk_widget_add_accelerator(btn_open,  "clicked", accel, GDK_KEY_o, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(btn_save,  "clicked", accel, GDK_KEY_s, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(btn_clear, "clicked", accel, GDK_KEY_l, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
}

/* ===== Construcción UI ===== */
static void activate(GtkApplication* app, gpointer user_data){
  (void)user_data;
  main_win = gtk_application_window_new(app);
  set_window_title(NULL);
  gtk_window_set_default_size(GTK_WINDOW(main_win), 900, 600);

  GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
  gtk_container_add(GTK_CONTAINER(main_win), vbox);

  GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

  /* panel izquierdo: input */
  GtkWidget *scr_in = gtk_scrolled_window_new(NULL, NULL);
  gtk_widget_set_hexpand(scr_in, TRUE);
  gtk_widget_set_vexpand(scr_in, TRUE);
  gtk_box_pack_start(GTK_BOX(hbox), scr_in, TRUE, TRUE, 0);

  txt_input = gtk_text_view_new();
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(txt_input), GTK_WRAP_NONE);
  gtk_container_add(GTK_CONTAINER(scr_in), txt_input);

  /* panel derecho: output */
  GtkWidget *scr_out = gtk_scrolled_window_new(NULL, NULL);
  gtk_widget_set_hexpand(scr_out, TRUE);
  gtk_widget_set_vexpand(scr_out, TRUE);
  gtk_box_pack_start(GTK_BOX(hbox), scr_out, TRUE, TRUE, 0);

  txt_output = gtk_text_view_new();
  gtk_text_view_set_editable(GTK_TEXT_VIEW(txt_output), FALSE);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(txt_output), GTK_WRAP_WORD_CHAR);
  gtk_container_add(GTK_CONTAINER(scr_out), txt_output);

  /* botones */
  GtkWidget *btn_box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
  gtk_box_pack_start(GTK_BOX(vbox), btn_box, FALSE, FALSE, 0);

  GtkWidget *btn_open   = gtk_button_new_with_label("Abrir .usl");
  GtkWidget *btn_save   = gtk_button_new_with_label("Guardar");
  GtkWidget *btn_analyze= gtk_button_new_with_label("Analizar & Ejecutar");
  GtkWidget *btn_clear  = gtk_button_new_with_label("Limpiar");

  gtk_box_pack_start(GTK_BOX(btn_box), btn_open,   FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(btn_box), btn_save,   FALSE, FALSE, 0);

  GtkWidget *spacer = gtk_label_new(NULL);
  gtk_widget_set_hexpand(spacer, TRUE);
  gtk_box_pack_start(GTK_BOX(btn_box), spacer, TRUE, TRUE, 0);

  gtk_box_pack_end(GTK_BOX(btn_box), btn_clear,   FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(btn_box), btn_analyze, FALSE, FALSE, 0);

  /* señales */
  g_signal_connect(main_win,   "destroy", G_CALLBACK(gtk_main_quit), NULL);
  g_signal_connect(btn_analyze,"clicked", G_CALLBACK(on_analyze), NULL);
  g_signal_connect(btn_clear,  "clicked", G_CALLBACK(on_clear), NULL);
  g_signal_connect(btn_open,   "clicked", G_CALLBACK(on_open), main_win);
  g_signal_connect(btn_save,   "clicked", G_CALLBACK(on_save), main_win);

  add_accelerators(main_win, btn_open, btn_save, btn_clear);

  gtk_widget_show_all(main_win);
}

int main(int argc, char **argv){
  setlocale(LC_ALL, "C");  /* asegura '.' como separador decimal */
  GtkApplication *app = gtk_application_new("com.usac.usl", G_APPLICATION_DEFAULT_FLAGS);
  g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);
  int status = g_application_run(G_APPLICATION(app), argc, argv);
  g_object_unref(app);
  g_clear_pointer(&current_path, g_free);
  return status;
}
