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

/* widgets globales */
static GtkWidget *txt_input;
static GtkWidget *txt_output;

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

static void on_clear(GtkButton *btn, gpointer user_data){
  (void)btn; (void)user_data;
  GtkTextBuffer *ib = gtk_text_view_get_buffer(GTK_TEXT_VIEW(txt_input));
  gtk_text_buffer_set_text(ib, "", -1);
  set_output("");
}

static void activate(GtkApplication* app, gpointer user_data){
  (void)user_data;
  GtkWidget *win = gtk_application_window_new(app);
  gtk_window_set_title(GTK_WINDOW(win), "USL Parser+Runner");
  gtk_window_set_default_size(GTK_WINDOW(win), 900, 600);

  GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
  gtk_container_add(GTK_CONTAINER(win), vbox);

  GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

  /* panel izquierdo: input */
  GtkWidget *scr_in = gtk_scrolled_window_new(NULL, NULL);
  gtk_widget_set_hexpand(scr_in, TRUE);
  gtk_widget_set_vexpand(scr_in, TRUE);
  gtk_box_pack_start(GTK_BOX(hbox), scr_in, TRUE, TRUE, 0);

  txt_input = gtk_text_view_new();
  gtk_container_add(GTK_CONTAINER(scr_in), txt_input);

  /* panel derecho: output */
  GtkWidget *scr_out = gtk_scrolled_window_new(NULL, NULL);
  gtk_widget_set_hexpand(scr_out, TRUE);
  gtk_widget_set_vexpand(scr_out, TRUE);
  gtk_box_pack_start(GTK_BOX(hbox), scr_out, TRUE, TRUE, 0);

  txt_output = gtk_text_view_new();
  gtk_text_view_set_editable(GTK_TEXT_VIEW(txt_output), FALSE);
  gtk_container_add(GTK_CONTAINER(scr_out), txt_output);

  /* botones */
  GtkWidget *btn_box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
  gtk_box_pack_start(GTK_BOX(vbox), btn_box, FALSE, FALSE, 0);

  GtkWidget *btn_analyze = gtk_button_new_with_label("Analizar & Ejecutar");
  g_signal_connect(btn_analyze, "clicked", G_CALLBACK(on_analyze), NULL);
  gtk_box_pack_start(GTK_BOX(btn_box), btn_analyze, FALSE, FALSE, 0);

  GtkWidget *btn_clear = gtk_button_new_with_label("Limpiar");
  g_signal_connect(btn_clear, "clicked", G_CALLBACK(on_clear), NULL);
  gtk_box_pack_start(GTK_BOX(btn_box), btn_clear, FALSE, FALSE, 0);

  gtk_widget_show_all(win);
}

int main(int argc, char **argv){
  setlocale(LC_ALL, "C");  /* <- asegura '.' como separador decimal */
  GtkApplication *app = gtk_application_new("com.usac.usl", G_APPLICATION_DEFAULT_FLAGS);
  g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);
  int status = g_application_run(G_APPLICATION(app), argc, argv);
  g_object_unref(app);
  return status;
}