#include "crsv.h"
#if     UNIX_V

#include <string.h>
#include <curses.h>

/* ====================  Externally Defined Functions  ==================== */

/* -----------------------------
 *  From the file: CRSVMISC.C
 * -----------------------------
 */

extern void end_crsv();


/* ====================  Externally Defined Variables  ==================== */

/* ====================  Internally Defined Functions  ==================== */

int     pc_window();
int     pc_error_message();
int     pc_send_message();
int     end_window();

void   init_screen();
void   exit_low();
void   exit_hi();
void   set_options();
void   copy_options();
void   set_item();
void   error_message();
void   make_list_screen();

/* ====================  Internally Defined Variables  ==================== */

#define H_CHAR   42
#define V_CHAR   42

#define ESC      27
#define CTRL_C    3
#define CTRL_D    4
#define CTRL_N   14
#define CTRL_P   16
#define CTRL_F    6
#define CTRL_B    2

#define border_attr    (A_NORMAL)
#define label_attr     (A_NORMAL)
#define item_attr      (A_NORMAL)
#define select_attr    (A_REVERSE)
#define line_attr      (A_NORMAL)
#define title_attr     (A_BOLD)
#define file_attr      (A_NORMAL)

#define SCROLL_MAX     20

int SCREEN_SET_FLAG = NO;
int MORE_FLAG;

WINDOW *screen;

/* ======================================================================== */

int pc_window(num_files, file_list, def_rel_name)
int  *num_files;
char  file_list[MAX_FILES][MAX_NAME], def_rel_name[];
{
   int quit_flag = NO;
   int c, opt, last_opt;
   int options[10];
   int rtn;

   SCREEN_SET_FLAG = YES;

   set_options(options);
   def_rel_name[0] = EOS;
   init_screen();

   last_opt = 7;
   opt= 8;
   exit_hi();

   while(quit_flag IS_NO) DO
      wrefresh(stdscr);
      c = wgetch(stdscr);

      switch (c) {
        case KEY_UP:
        case CTRL_P:
             last_opt = opt;
             opt -= 1;
             IF(opt < 1) THEN
                opt = 8;
             END_IF
             break;

        case KEY_DOWN:
        case CTRL_N:
             last_opt = opt;
             opt += 1;
             IF(opt > 8) THEN
                opt = 1;
             END_IF
                   break;

        case ESC:
        case CTRL_C:
             endwin();
             return(ERROR);

        case KEY_LEFT:
        case KEY_RIGHT:
        case CTRL_B:
        case CTRL_F:
             IF(opt EQ 8) THEN
                exit_low();
             ELSE
                set_item(options, opt, item_attr);
             END_IF
             rtn = process_file_list(num_files, file_list);
             IF(rtn EQ ERROR) THEN
                endwin();
                return(ERROR);
             END_IF
             break;

        case RETURN:
        case NEWLINE:
        case SPACE:
        case 'Y':
        case 'y':
        case 'N':
        case 'n':
             IF((opt EQ 8) AND (*num_files <= 0)) THEN
                error_message("Must input at least one file name");
             ELSE_IF(opt EQ 8)
                quit_flag = YES;
             ELSE_IF(options[opt] EQ YES)
                options[opt] = NO;
             ELSE_IF(options[opt] EQ NO)
                options[opt] = YES;
             END_IF

             IF((options[5] EQ YES) AND (options[7] EQ YES)) THEN
                error_message("Can't Verify and Create at the same time!");
                options[opt] = NO;
             END_IF

             IF((options[7] EQ YES) AND (opt EQ 7)) THEN
                set_item(options, opt, select_attr);
                input_def_rel_name(def_rel_name);
             END_IF
             break;

        default:
             break;
        }

      IF((opt NEQ 8) AND (last_opt NEQ 8)) THEN
         set_item(options, last_opt, item_attr);
         set_item(options, opt, select_attr);
      ELSE_IF(last_opt EQ 8)
         exit_low();
         set_item(options, opt, select_attr);
      ELSE_IF(opt EQ 8)
         set_item(options, last_opt, item_attr);
         exit_hi();
      END_IF

   END_WHILE

   copy_options(options);
   make_list_screen();
   return(OK);
}

/* ======================================================================== */

static void init_screen()
{
   int row;

   initscr();
   nonl();
   noecho();
   raw();
   cbreak();
   keypad(stdscr, TRUE);

   /*==============*/
   /* Init display */
   /*==============*/

   box(stdscr, V_CHAR, H_CHAR);
   wattrset(stdscr, title_attr);

   mvwaddstr(stdscr,  1, 18, "CLIPS Style Analysis and Verification Tool");
   wattrset(stdscr, label_attr);

   mvwaddstr(stdscr,  4, 9, "Be Verbose");

   mvwaddstr(stdscr,  6, 9, "Cross Reference Relations");

   mvwaddstr(stdscr,  8, 9, "Style Warnings and Analysis");

   mvwaddstr(stdscr, 10, 9, "Rule/Deffact Summaries");

   mvwaddstr(stdscr, 12, 9, "Verify Rules with Defrelations");

   mvwaddstr(stdscr, 14, 9, "Cross Reference User Functions");

   mvwaddstr(stdscr, 16, 9, "Create Defrelations File");

   /*===================*/
   /* Init option flags */
   /*===================*/

   wattrset(stdscr, item_attr);

   IF(VERBOSE IS_ON) THEN
      mvwaddstr(stdscr,  4, 4, "YES");
   ELSE
      mvwaddstr(stdscr,  4, 4, "NO");
   END_IF

   IF(CHECK_RELATIONS IS_ON) THEN
      mvwaddstr(stdscr,  6, 4, "YES");
   ELSE
      mvwaddstr(stdscr,  6, 4, "NO");
   END_IF

   IF(CHECK_STYLE IS_ON) THEN
      mvwaddstr(stdscr,  8, 4, "YES");
   ELSE
      mvwaddstr(stdscr,  8, 4, "NO");
   END_IF

   IF(CHECK_RULES IS_ON) THEN
      mvwaddstr(stdscr, 10, 4, "YES");
   ELSE
      mvwaddstr(stdscr, 10, 4, "NO");
   END_IF

   IF(CHECK_DEFRELS   IS_ON) THEN
      mvwaddstr(stdscr,  12, 4, "YES");
   ELSE
      mvwaddstr(stdscr,  12, 4, "NO");
   END_IF

   IF(CHECK_EX_FLAG IS_ON) THEN
      mvwaddstr(stdscr,  14, 4, "YES");
   ELSE
      mvwaddstr(stdscr,  14, 4, "NO");
   END_IF

   IF(CREATE_DEFRELS  IS_ON) THEN
      mvwaddstr(stdscr,  16, 4, "YES");
   ELSE
      mvwaddstr(stdscr,  16, 4, "NO");
   END_IF

   /*==============================*/
   /* Create, Display Def-Rel line */
   /*==============================*/

   wattrset(stdscr, label_attr);
   mvwaddstr(stdscr, 19,  9, "File to store Defrelations:");
   wattrset(stdscr, line_attr);
   mvwaddstr(stdscr, 20,  7, "_______________________________");

   /*================*/
   /* Init file list */
   /*================*/

   wattrset(stdscr, label_attr);

   mvwaddstr(stdscr, 3, 51, "List of Input Files");

   wattrset(stdscr, line_attr);
   for(row = 4; row < SCROLL_MAX + 1; row++) DO
      mvwaddstr(stdscr, row, 45, "_______________________________");
   DONE

   exit_low();

   touchwin(stdscr);
   wrefresh(stdscr);
}

/* ======================================================================== */

static void exit_low()
{
   wattrset(stdscr, item_attr);

   mvwaddstr(stdscr, SCROLL_MAX + 2, 30, " Execute Program ");
}

/* ======================================================================== */

static void exit_hi()
{
   wattrset(stdscr, select_attr);

   mvwaddstr(stdscr, SCROLL_MAX + 2, 30, " Execute Program ");
}

/* ======================================================================== */

static void set_item(list, item, color)
int list[], item;
int color;
{
   wattrset(stdscr, color);

   IF(list[item] IS_ON) THEN
      mvwaddstr(stdscr,  (item * 2) + 2, 3, " YES");
   ELSE
      mvwaddstr(stdscr,  (item * 2) + 2, 3, " NO ");
   END_IF
}

/* ======================================================================== */

static void set_options(list)
int list[];
{
   list[1] = VERBOSE;
   list[2] = CHECK_RELATIONS;
   list[3] = CHECK_STYLE;
   list[4] = CHECK_RULES;
   list[5] = CHECK_DEFRELS;
   list[6] = CHECK_EX_FLAG;
   list[7] = CREATE_DEFRELS;
}

/* ======================================================================== */

static void copy_options(list)
int list[];
{
   VERBOSE         = list[1];
   CHECK_RELATIONS = list[2];
   CHECK_STYLE     = list[3];
   CHECK_RULES     = list[4];
   CHECK_DEFRELS   = list[5];
   CHECK_EX_FLAG   = list[6];
   CREATE_DEFRELS  = list[7];
}

/* ======================================================================== */

static void error_message(str)
char *str;
{
   wattrset(stdscr, title_attr);
   mvwaddstr(stdscr, SCROLL_MAX + 2, 10,
         "                                                         ");

   mvwaddstr(stdscr, SCROLL_MAX + 2, (78 - strlen(str)) / 2, str);
   wrefresh(stdscr);
   sleep(2);
   wattrset(stdscr, item_attr);
   mvwaddstr(stdscr, SCROLL_MAX + 2, 10,
         "                                                         ");
   exit_low();
}

/* ======================================================================== */

static int input_def_rel_name(def_rel_name)
char *def_rel_name;
{
   int    exit_flag = NO;
   int    diff;
   char   temp[40];

   wattrset(stdscr, line_attr);
   mvwaddstr(stdscr, 20, 4, "->");
   echo();
   while(exit_flag IS_NO) DO
      wattrset(stdscr, file_attr);
      mvwgetstr(stdscr, 20, 7, temp);
      IF(strlen(temp) > 0) THEN
         (void)strcpy(def_rel_name, temp);
         exit_flag = YES;
      ELSE
         error_message("Must enter a file name");
      END_IF
   END_WHILE
   noecho();
   mvwaddstr(stdscr, 20, 4, "  ");
   mvwaddstr(stdscr, 20, 7, temp);
   diff = 31 - strlen(def_rel_name);
   IF(diff > 0) THEN
      wattrset(stdscr, line_attr);
      for(; diff > 0; diff--) DO
         waddch(stdscr, '_');
      DONE
   END_IF
}

/* ======================================================================== */

static int process_file_list(num_files, file_list)
int *num_files;
char file_list[][40];
{
   int row, min_row;
   int col, max_row;
   int quit_flag = NO;
   int c, rtn, i;

   min_row = row = 4;
   max_row = min_row + *num_files;
   col     = 42;

   while(quit_flag IS_NO) DO
      wattrset(stdscr, item_attr);
      mvwaddstr(stdscr, row, col, "-> ");
      wrefresh(stdscr);
      c = wgetch(stdscr);

      switch (c) {
        case KEY_UP:
        case CTRL_P:
             mvwaddstr(stdscr, row, col, "   ");
             row -= 1;
             IF(row < min_row) THEN
                row = max_row;
             END_IF
             break;

        case KEY_DOWN:
        case CTRL_N:
             mvwaddstr(stdscr, row, col, "   ");
             row += 1;
             IF(row > max_row) THEN
                row = min_row;
             END_IF
                   break;

        case ESC:
        case CTRL_C:
             mvwaddstr(stdscr, row, col, "   ");
             return(ERROR);

        case KEY_LEFT:
        case KEY_RIGHT:
        case CTRL_F:
        case CTRL_B:
             mvwaddstr(stdscr, row, col, "   ");
             return(OK);

        case RETURN:
        case NEWLINE:
             mvwaddstr(stdscr, row, col, "   ");
             i = row - min_row;
             rtn = input_file_name(file_list[i]);
             IF(row EQ max_row) THEN
               (*num_files)++;
                max_row++;
             END_IF
             row++;
             break;

        case CTRL_D:
        case KEY_DC:
             mvwaddstr(stdscr, row, col, "   ");
             i = row - min_row;
             rtn = delete_file(file_list, i, *num_files);
             (*num_files)--;
             max_row--;
             break;

        default:
             break;
        }
   END_WHILE

   return(OK);
}

/* ======================================================================== */

static int input_file_name(name)
char name[];
{
   int row, col;
   int    exit_flag = NO;
   int    diff;
   char   temp[40];

   getyx(stdscr, row, col);
   echo();
   while(exit_flag IS_NO) DO
      wattrset(stdscr, file_attr);
      mvwgetstr(stdscr, row, col, temp);
      IF(strlen(temp) > 0) THEN
         (void)strcpy(name, temp);
         exit_flag = YES;
      ELSE
         error_message("Must enter a file name");
      END_IF
   END_WHILE
   noecho();
   mvwaddstr(stdscr, row, col, name);
   diff = 31 - strlen(name);
   wattrset(stdscr, line_attr);
   for(; diff > 0; diff--) DO
      waddch(stdscr, '_');
   DONE

   return(YES);
}

/* ======================================================================== */

static int delete_file(list, now, max)
int now, max;
char list[][40];
{
   int row, col;
   int diff;

   max--;
   getyx(stdscr, row, col);
   IF(now EQ max) THEN
      wattrset(stdscr, line_attr);
      mvwaddstr(stdscr, row, 45, "_______________________________");
   ELSE
      (void)strcpy(list[now], list[max]);
      wattrset(stdscr, file_attr);
      mvwaddstr(stdscr, row, 45, list[now]);
      diff = 31 - strlen(list[now]);
      wattrset(stdscr, line_attr);
      for(; diff > 0; diff--) DO
         waddch(stdscr, '_');
      DONE
      diff = max - now;
      mvwaddstr(stdscr, row + diff, 45, "_______________________________");
   END_IF

   return(OK);
}

/* ======================================================================== */

void make_list_screen()
{
   wclear(stdscr);
   box(stdscr, V_CHAR, H_CHAR);

   screen = newwin(SCROLL_MAX + 2, 78, 1, 1);
   scrollok(screen, TRUE);
   wsetscrreg(screen, 0, SCROLL_MAX);

   touchwin(stdscr);
   refresh();
   touchwin(screen);
   wrefresh(screen);
   MORE_FLAG = 0;
}

/* ======================================================================== */

int end_window()
{
   endwin();
}

/* ======================================================================== */

int pc_pause()
{
   int c;

   wattrset(screen, title_attr);
   wmove(screen, SCROLL_MAX + 1, 0);
   wclrtoeol(screen);
   mvwaddstr(screen, SCROLL_MAX + 1, 34, "-- More --");
   wrefresh(screen);
   c = wgetch(screen);
   wattrset(screen, item_attr);
   wmove(screen, SCROLL_MAX + 1, 0);
   wclrtoeol(screen);
   wmove(screen, SCROLL_MAX,0);
   IF((c EQ NEWLINE) OR (c EQ RETURN)) THEN
      return(SCROLL_MAX - 1);
   ELSE_IF((c EQ 'Q') OR (c EQ 'q'))
      end_crsv();
   ELSE
      return(0);
   END_IF
}

/* ======================================================================== */

int pc_error_message(str)
char *str;
{
   int row, col, i;

   IF(SCREEN_SET_FLAG IS_ON) THEN
      IF(str[1] EQ 'E') THEN
         wattrset(screen, item_attr);
      ELSE
         wattrset(screen, file_attr);
      END_IF

      i = 0;
      while(str[i] NEQ EOS) DO
         IF(str[i] EQ NEWLINE) THEN
            MORE_FLAG++;
            IF(MORE_FLAG EQ SCROLL_MAX) THEN
               MORE_FLAG = pc_pause();
            END_IF
            getyx(screen, row, col);
            IF(row EQ SCROLL_MAX) THEN
               scroll(screen);
               wmove(screen, row, 0);
            ELSE
               wmove(screen, row+1, 0);
            END_IF

         ELSE_IF(str[i] EQ ':')
            waddch(screen, str[i]);
            wattrset(screen, line_attr);
         ELSE
            waddch(screen, str[i]);
         END_IF
         i++;
      DONE

      wrefresh(screen);
   ELSE
      (void)printf("%s", str);
   END_IF
}

/* ======================================================================== */

int pc_send_message(str)
char *str;
{
   int row, col, i;

   IF(SCREEN_SET_FLAG IS_ON) THEN
      wattrset(screen, line_attr);

      i = 0;
      while(str[i] NEQ EOS) DO
         IF(str[i] EQ NEWLINE) THEN
            MORE_FLAG++;
            IF(MORE_FLAG EQ SCROLL_MAX) THEN
               MORE_FLAG = pc_pause();
            END_IF
            getyx(screen, row, col);
            IF(row EQ SCROLL_MAX) THEN
               scroll(screen);
               wmove(screen, row, 0);
            ELSE
               wmove(screen, row+1, 0);
            END_IF

         ELSE
           waddch(screen, str[i]);
         END_IF
         i++;
      DONE

      wrefresh(screen);
   ELSE
      (void)printf("%s", str);
   END_IF
}

#endif
