#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <vector>
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <lo/lo.h>

// ----------------------------- declarations -------------------------

struct video_control;
struct audio_control;

enum graph_mode
{
  mode_create_node,
  mode_delete_node,
  mode_move_node,
  mode_create_link,
  mode_delete_link,
  mode_flip_link,
  mode_target_link,
  mode_help
};

struct graph_node;
struct graph_link;
struct graph_arrow;
struct graph_cord;
struct graph_designer;

typedef bool (*node_mouse_t)(graph_node *, double, double);
typedef bool (*link_mouse_t)(graph_link *, double, double);
typedef bool (*arrow_mouse_t)(graph_arrow *, double, double);
typedef bool (*cord_mouse_t)(graph_cord *, double, double);
typedef bool (*designer_mouse_t)(graph_designer *, double, double);

graph_link *link_new(graph_designer *designer, graph_node *from, graph_node *to);
void link_delete(graph_link *link);
void link_leave_mode(graph_link *link);
void link_enter_mode(graph_link *link, graph_mode mode);
void link_update(graph_link *link);
graph_arrow *arrow_new();
void arrow_delete(graph_arrow *arrow);
void arrow_leave_mode(graph_arrow *arrow);
void arrow_enter_mode(graph_arrow *arrow, graph_mode mode);
graph_cord *cord_new(double x1, double y1, double x2, double y2);
void cord_delete(graph_cord *cord);
graph_node *node_new(graph_designer *designer, double x, double y, bool fixed);
void node_delete(graph_node *node);
bool nodes_linked(graph_node *a, graph_node *b);
void node_leave_mode(graph_node *node);
bool node_mouse_hit(graph_node *node, double x, double y);
bool node_mouse_down_new(graph_node *node, double x, double y);
bool node_mouse_down_delete(graph_node *node, double x, double y);
bool node_mouse_down_move(graph_node *node, double x, double y);
bool node_mouse_down_link(graph_node *node, double x, double y);
bool node_mouse_up_corded(graph_node *node, double x, double y);
void node_enter_mode(graph_node *node, graph_mode mode);
graph_designer *designer_new(int id);
void designer_leave_mode(graph_designer *designer);
void designer_enter_mode(graph_designer *designer, graph_mode mode);
void node_move_by(graph_node *node, double dx, double dy);
bool designer_mouse_down(graph_designer *designer, double x, double y);
bool designer_mouse_down_node_new(graph_designer *designer, double x, double y);
bool designer_mouse_move_dragging(graph_designer *designer, double x, double y);
bool designer_mouse_move_dragged(graph_designer *designer, double x, double y);
bool designer_mouse_move_cording(graph_designer *designer, double x, double y);
bool designer_mouse_move_corded(graph_designer *designer, double x, double y);

// ----------------------------- constants ----------------------------

const glm::vec3 white = glm::vec3(1.0f);
const glm::vec3 black = glm::vec3(0.0f);
const glm::vec3 colours[4] =
  { glm::vec3(1.0f, 0.5f, 0.0f)
  , glm::vec3(0.5f, 1.0f, 0.0f)
  , glm::vec3(0.0f, 0.5f, 1.0f)
  , glm::vec3(0.5f, 0.0f, 1.0f)
  };

const double minimum_x = 50;
const double minimum_y = 50;
const double maximum_x = 750;
const double maximum_y = 750;

// ----------------------------- controls -----------------------------

struct video_control
{
  int32_t active;
  int32_t count    [4];
  int32_t source   [4][8];
  float   scale    [4][8];
  float   transform[4][8][3][3];
};

struct audio_control
{
  float   scale[4][4][8];
  float   pan  [4][4][8];
  uint8_t level[4][4][8];
};

// ----------------------------- structs ------------------------------

struct graph_link
{
  graph_designer *designer;
  graph_node *from, *to;
  double x1, y1, x2, y2;
  int target;
  bool flipped;
  graph_arrow *arrow;
  link_mouse_t on_mouse_down, on_mouse_move, on_mouse_up;
};

struct graph_arrow
{
  graph_link *link;
  double x1, y1, x2, y2, x3, y3;
  arrow_mouse_t on_mouse_down, on_mouse_move, on_mouse_up;
};

struct graph_cord
{
  double x1, y1, x2, y2;
};

struct graph_node
{
  graph_designer *designer;
  std::vector< graph_link * > link1, link2;
  bool fixed;
  double x;
  double y;
  double dragx;
  double dragy;
  node_mouse_t on_mouse_down, on_mouse_move, on_mouse_up;
};

struct graph_designer
{
  int id;
  graph_node *start_node;
  graph_node *end_node;
  graph_node *dragging_node;
  graph_node *cording_node;
  graph_cord *cord;
  std::vector< graph_link  * >  links;
  std::vector< graph_arrow * > arrows;
  std::vector< graph_node  * >  nodes;
  designer_mouse_t on_mouse_down, on_mouse_move, on_mouse_up;
};

struct graph_draw
{
  int components;
  float tri[4096];
  GLuint vbo_tri;
  GLuint vao_tri;
  GLuint vao_quad;
  GLuint prog_circle;
  GLuint prog_arrow;
  GLuint prog_line;
  GLuint prog_buttons;
  GLuint prog_help;
  GLuint tex[3];
  GLint u_mode;
};

// ----------------------------- link ---------------------------------

graph_link *link_new(graph_designer *designer, graph_node *from, graph_node *to)
{
  graph_link *link = new graph_link();
  link->designer = designer;
  link->from = from;
  link->to = to;
  link->x1 = from->x;
  link->y1 = from->y;
  link->x2 = to->x;
  link->y2 = to->y;
  link->target = designer->id;
  link->arrow = new graph_arrow();
  link->arrow->link = link;
  from->link1.push_back(link);
  to->link2.push_back(link);
  designer->links.push_back(link);
  designer->arrows.push_back(link->arrow);
  link_update(link);
  return link;
}

void link_delete(graph_link *link)
{
  for (int i = 0; i < link->from->link1.size(); )
    if (link == link->from->link1[i])
      link->from->link1.erase(link->from->link1.begin() + i);
    else
      ++i;
  for (int i = 0; i < link->to->link2.size(); )
    if (link == link->to->link2[i])
      link->to->link2.erase(link->to->link2.begin() + i);
    else
      ++i;
  for (int i = 0; i < link->designer->links.size(); )
    if (link == link->designer->links[i])
      link->designer->links.erase(link->designer->links.begin() + i);
    else
      ++i;
  for (int i = 0; i < link->designer->arrows.size(); )
    if (link->arrow == link->designer->arrows[i])
      link->designer->arrows.erase(link->designer->arrows.begin() + i);
    else
      ++i;
  arrow_delete(link->arrow);
  link->arrow = 0;
  link->from = 0;
  link->to = 0;
  link->designer = 0;
  delete link;
}

void link_leave_mode(graph_link *link)
{
  (void) link;
}

void link_enter_mode(graph_link *link, graph_mode mode)
{
  (void) link;
  (void) mode;
}

void link_update(graph_link *link)
{
  const double w = 30;
  const double c = w * -0.5;
  const double s = w * 0.8660254037844387;
  double x1 = link->x1;
  double y1 = link->y1;
  double x2 = link->x2;
  double y2 = link->y2;
  double dx = x2 - x1;
  double dy = y2 - y1;
  if (link->flipped)
  {
    dx = -dx;
    dy = -dy;
  }
  double r = sqrt(dx * dx + dy * dy);
  dx /= r;
  dy /= r;
  double ox = (x1 + x2) / 2;
  double oy = (y1 + y2) / 2;
  link->arrow->x1 = ox + w * dx;
  link->arrow->y1 = oy + w * dy;
  link->arrow->x2 = ox + c * dx + s * dy;
  link->arrow->y2 = oy - s * dx + c * dy;
  link->arrow->x3 = ox + c * dx - s * dy;
  link->arrow->y3 = oy + s * dx + c * dy;
}

// ----------------------------- arrow --------------------------------

graph_arrow *arrow_new()
{
  graph_arrow *arrow = new graph_arrow();
  arrow->on_mouse_down = 0;
  arrow->on_mouse_move = 0;
  arrow->on_mouse_up   = 0;
  return arrow;
}

void arrow_delete(graph_arrow *arrow)
{
  delete arrow;
}

void arrow_leave_mode(graph_arrow *arrow)
{
  arrow->on_mouse_down = 0;
  arrow->on_mouse_move = 0;
  arrow->on_mouse_up   = 0;
}

bool arrow_hit(graph_arrow *arrow, double x, double y)
{
  double x0 = (arrow->x1 + arrow->x2 + arrow->x3) / 3;
  double y0 = (arrow->y1 + arrow->y2 + arrow->y3) / 3;
  double dx = x - x0;
  double dy = y - y0;
  double r2 = dx * dx + dy * dy;
  return r2 < 500;
}

bool arrow_mouse_down_delete(graph_arrow *arrow, double x, double y)
{
  if (arrow_hit(arrow, x, y))
  {
    link_delete(arrow->link);
    // arrow is deleted
    return true;
  }
  return false;
}

bool arrow_mouse_down_flip(graph_arrow *arrow, double x, double y)
{
  if (arrow_hit(arrow, x, y))
  {
    arrow->link->flipped = !(arrow->link->flipped);
    link_update(arrow->link);
    return true;
  }
  return false;
}

bool arrow_mouse_down_target(graph_arrow *arrow, double x, double y)
{
  if (arrow_hit(arrow, x, y))
  {
    arrow->link->target = (arrow->link->target + 1) % 4;
    link_update(arrow->link);
    return true;
  }
  return false;
}

void arrow_enter_mode(graph_arrow *arrow, graph_mode mode)
{
  switch (mode)
  {
    case mode_delete_link:
      arrow->on_mouse_down = arrow_mouse_down_delete;
      break;
    case mode_flip_link:
      arrow->on_mouse_down = arrow_mouse_down_flip;
      break;
    case mode_target_link:
      arrow->on_mouse_down = arrow_mouse_down_target;
      break;
    case mode_create_link:
    case mode_create_node:
    case mode_delete_node:
    case mode_move_node:
    case mode_help:
      break;
  }
}

// ----------------------------- cord ---------------------------------

graph_cord *cord_new(double x1, double y1, double x2, double y2)
{
  graph_cord *cord = new graph_cord();
  cord->x1 = x1;
  cord->y1 = y1;
  cord->x2 = x2;
  cord->y2 = y2;
  return cord;
}

void cord_delete(graph_cord *cord)
{
  delete cord;
}

// ----------------------------- node ---------------------------------

graph_node *node_new(graph_designer *designer, double x, double y, bool fixed)
{
  graph_node *node = new graph_node();
  node->fixed = fixed;
  node->x = fmin(fmax(x, minimum_x), maximum_x);
  node->y = fmin(fmax(y, minimum_y), maximum_y);
  node->dragx = node->x;
  node->dragy = node->y;
  node->designer = designer;
  designer->nodes.push_back(node);
  return node;
}

void node_delete(graph_node *node)
{
  graph_designer *designer = node->designer;
  for (int i = 0; i < designer->nodes.size(); )
    if (node == designer->nodes[i])
      designer->nodes.erase(designer->nodes.begin() + i);
    else
      ++i;
  while (node->link1.size())
    link_delete(node->link1[0]);
  while (node->link2.size())
    link_delete(node->link2[0]);
  node->designer = 0;
  delete node;
}

bool nodes_linked(graph_node *a, graph_node *b)
{
  for (int i = 0; i < a->link1.size(); ++i)
    if (a->link1[i]->to == b)
      return true;
  for (int i = 0; i < b->link1.size(); ++i)
    if (b->link1[i]->to == a)
      return true;
  return false;
}

void node_leave_mode(graph_node *node)
{
  node->on_mouse_down = 0;
  node->on_mouse_move = 0;
  node->on_mouse_up   = 0;
}

bool node_mouse_hit(graph_node *node, double x, double y)
{
  double dx = x - node->x;
  double dy = y - node->y;
  double r2 = dx * dx + dy * dy;
  return r2 < 500;
}

bool node_mouse_down_new(graph_node *node, double x, double y)
{
  return node_mouse_hit(node, x, y);
}

bool node_mouse_down_delete(graph_node *node, double x, double y)
{
  if (node_mouse_hit(node, x, y))
  {
    node_delete(node);
    return true;
  }
  return false;  
}

bool node_mouse_down_move(graph_node *node, double x, double y)
{
  if (node_mouse_hit(node, x, y))
  {
    node->designer->dragging_node = node;
    node->designer->on_mouse_move = designer_mouse_move_dragging;
    node->designer->on_mouse_up   = designer_mouse_move_dragged;
    node->dragx = fmin(fmax(x, minimum_x), maximum_x);
    node->dragy = fmin(fmax(y, minimum_y), maximum_y);
    return true;
  }
  return false;
}

bool node_mouse_down_link(graph_node *node, double x, double y)
{
  if (node_mouse_hit(node, x, y) && node->designer->links.size() < 8)
  {
    node->designer->cording_node = node;
    node->designer->cord = cord_new(node->x, node->y, x, y);
    for (int i = 0; i < node->designer->nodes.size(); ++i)
      if (node != node->designer->nodes[i] && ! nodes_linked(node, node->designer->nodes[i]))
        node->designer->nodes[i]->on_mouse_up = node_mouse_up_corded;
    node->designer->on_mouse_move = designer_mouse_move_cording;
    node->designer->on_mouse_up   = designer_mouse_move_corded;
    return true;
  }
  return false;
}

bool node_mouse_up_corded(graph_node *node, double x, double y)
{
  if (node_mouse_hit(node, x, y))
  {
    link_new(node->designer, node->designer->cording_node, node);
    node->designer->cording_node = 0;
    delete node->designer->cord;
    node->designer->cord = 0;
    for (int i = 0; i < node->designer->nodes.size(); ++i)
      node->designer->nodes[i]->on_mouse_up = 0;
    node->designer->on_mouse_move = 0;
    node->designer->on_mouse_up   = 0;
    return true;
  }
  return false;
}

void node_enter_mode(graph_node *node, graph_mode mode)
{
  switch (mode)
  {
    case mode_create_node:
      node->on_mouse_down = node_mouse_down_new;
      break;
    case mode_delete_node:
      if (! node->fixed)
        node->on_mouse_down = node_mouse_down_delete;
      break;
    case mode_move_node:
      if (! node->fixed)
        node->on_mouse_down = node_mouse_down_move;
      break;
    case mode_create_link:
      node->on_mouse_down = node_mouse_down_link;
      break;
    case mode_delete_link:
    case mode_flip_link:
    case mode_target_link:
    case mode_help:
      break;
  }
}

// ----------------------------- designer -----------------------------

graph_designer *designer_new(int id)
{
  graph_designer *designer = new graph_designer();
  designer->id = id;
  designer->start_node = node_new(designer, 100, 400, true);
  designer->end_node   = node_new(designer, 700, 400, true);
  designer->dragging_node = 0;
  designer->cording_node = 0;
  designer->on_mouse_down = 0;
  designer->on_mouse_move = 0;
  designer->on_mouse_up   = 0;
  return designer;
}

void designer_leave_mode(graph_designer *designer)
{
  for (int i = 0; i < designer->links.size(); ++i)
    link_leave_mode(designer->links[i]);
  for (int i = 0; i < designer->arrows.size(); ++i)
    arrow_leave_mode(designer->arrows[i]);
  for (int i = 0; i < designer->nodes.size(); ++i)
    node_leave_mode(designer->nodes[i]);
  designer->on_mouse_down = 0;
  designer->on_mouse_move = 0;
  designer->on_mouse_up   = 0;
}

void designer_enter_mode(graph_designer *designer, graph_mode mode)
{
  for (int i = 0; i < designer->links.size(); ++i)
    link_enter_mode(designer->links[i], mode);
  for (int i = 0; i < designer->arrows.size(); ++i)
    arrow_enter_mode(designer->arrows[i], mode);
  for (int i = 0; i < designer->nodes.size(); ++i)
    node_enter_mode(designer->nodes[i], mode);
  switch (mode)
  {
    case mode_create_node:
      designer->on_mouse_down = designer_mouse_down_node_new;
      break;
    case mode_delete_node:
    case mode_move_node:
    case mode_create_link:
    case mode_delete_link:
    case mode_flip_link:
    case mode_target_link:
    case mode_help:
      break;
  }
}

void node_move_by(graph_node *node, double dx, double dy)
{
  for (int i = 0; i < node->link2.size(); ++i)
  {
    node->link2[i]->x2 = fmin(fmax(node->link2[i]->x2 + dx, minimum_x), maximum_x);
    node->link2[i]->y2 = fmin(fmax(node->link2[i]->y2 + dy, minimum_y), maximum_y);
    link_update(node->link2[i]);
  }
  for (int i = 0; i < node->link1.size(); ++i)
  {
    node->link1[i]->x1 = fmin(fmax(node->link1[i]->x1 + dx, minimum_x), maximum_x);
    node->link1[i]->y1 = fmin(fmax(node->link1[i]->y1 + dy, minimum_y), maximum_y);
    link_update(node->link1[i]);
  }
  node->x = fmin(fmax(node->x + dx, minimum_x), maximum_x);
  node->y = fmin(fmax(node->y + dy, minimum_y), maximum_y);
}

bool designer_mouse_down(graph_designer *designer, double x, double y)
{
  for (int i = 0; i < designer->nodes.size(); ++i)
    if (designer->nodes[i]->on_mouse_down)
      if (designer->nodes[i]->on_mouse_down(designer->nodes[i], x, y))
        return true;
  for (int i = 0; i < designer->arrows.size(); ++i)
    if (designer->arrows[i]->on_mouse_down)
      if (designer->arrows[i]->on_mouse_down(designer->arrows[i], x, y))
        return true;
  if (designer->on_mouse_down)
    return designer->on_mouse_down(designer, x, y);
  return false;
}

bool designer_mouse_move(graph_designer *designer, double x, double y)
{
  for (int i = 0; i < designer->nodes.size(); ++i)
    if (designer->nodes[i]->on_mouse_move)
      if (designer->nodes[i]->on_mouse_move(designer->nodes[i], x, y))
        return true;
  if (designer->on_mouse_move)
    return designer->on_mouse_move(designer, x, y);
  return false;
}

bool designer_mouse_up(graph_designer *designer, double x, double y)
{
  for (int i = 0; i < designer->nodes.size(); ++i)
    if (designer->nodes[i]->on_mouse_up)
      if (designer->nodes[i]->on_mouse_up(designer->nodes[i], x, y))
        return true;
  if (designer->on_mouse_up)
    return designer->on_mouse_up(designer, x, y);
  return false;
}

bool designer_mouse_down_node_new(graph_designer *designer, double x, double y)
{
  if (minimum_x <= x && x <= maximum_x && minimum_y <= y && y <= maximum_y && designer->nodes.size() < 18)
  {
    graph_node *node = node_new(designer, x, y, false);
    node_enter_mode(node, mode_create_node);
  }
  return true;
}

bool designer_mouse_move_dragging(graph_designer *designer, double x, double y)
{
  double dx = fmin(fmax(x, minimum_x), maximum_x) - designer->dragging_node->dragx;
  double dy = fmin(fmax(y, minimum_y), maximum_y) - designer->dragging_node->dragy;
  designer->dragging_node->dragx = fmin(fmax(x, minimum_x), maximum_x);
  designer->dragging_node->dragy = fmin(fmax(y, minimum_y), maximum_y);
  node_move_by(designer->dragging_node, dx, dy);
  return true;
}

bool designer_mouse_move_dragged(graph_designer *designer, double x, double y)
{
  designer_mouse_move_dragging(designer, x, y);
  designer->on_mouse_move = 0;
  designer->on_mouse_up = 0;
  designer->dragging_node = 0;
  return true;
}

bool designer_mouse_move_cording(graph_designer *designer, double x, double y)
{
  designer->cord->x2 = fmin(fmax(x, minimum_x), maximum_x);
  designer->cord->y2 = fmin(fmax(y, minimum_y), maximum_y);
  return true;
}

bool designer_mouse_move_corded(graph_designer *designer, double x, double y)
{
  for (int i = 0; i < designer->nodes.size(); ++i)
    designer->nodes[i]->on_mouse_up = 0;
  designer->on_mouse_move = 0;
  designer->on_mouse_up = 0;
  designer->cording_node = 0;
  delete designer->cord;
  designer->cord = 0;
  return true;
  (void) x;
  (void) y;
}

// ----------------------------- drawing ------------------------------

int draw_enqueue_triangle(graph_draw *draw, int k, double x1, double y1, double x2, double y2, double x3, double y3, glm::vec3 colour)
{
  draw->tri[k++] = x1;
  draw->tri[k++] = y1;
  draw->tri[k++] = 1;
  draw->tri[k++] = 0;
  draw->tri[k++] = 0;
  draw->tri[k++] = colour[0];
  draw->tri[k++] = colour[1];
  draw->tri[k++] = colour[2];
  draw->tri[k++] = x2;
  draw->tri[k++] = y2;
  draw->tri[k++] = 0;
  draw->tri[k++] = 1;
  draw->tri[k++] = 0;
  draw->tri[k++] = colour[0];
  draw->tri[k++] = colour[1];
  draw->tri[k++] = colour[2];
  draw->tri[k++] = x3;
  draw->tri[k++] = y3;
  draw->tri[k++] = 0;
  draw->tri[k++] = 0;
  draw->tri[k++] = 1;
  draw->tri[k++] = colour[0];
  draw->tri[k++] = colour[1];
  draw->tri[k++] = colour[2];
  return k;
}


int draw_enqueue_quad(graph_draw *draw, int k, double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4, glm::vec3 colour)
{
  draw->tri[k++] = x1;
  draw->tri[k++] = y1;
  draw->tri[k++] = 0;
  draw->tri[k++] = 0;
  draw->tri[k++] = colour[0];
  draw->tri[k++] = colour[1];
  draw->tri[k++] = colour[2];
  draw->tri[k++] = x2;
  draw->tri[k++] = y2;
  draw->tri[k++] = 0;
  draw->tri[k++] = 1;
  draw->tri[k++] = colour[0];
  draw->tri[k++] = colour[1];
  draw->tri[k++] = colour[2];
  draw->tri[k++] = x3;
  draw->tri[k++] = y3;
  draw->tri[k++] = 1;
  draw->tri[k++] = 0;
  draw->tri[k++] = colour[0];
  draw->tri[k++] = colour[1];
  draw->tri[k++] = colour[2];
  draw->tri[k++] = x3;
  draw->tri[k++] = y3;
  draw->tri[k++] = 1;
  draw->tri[k++] = 0;
  draw->tri[k++] = colour[0];
  draw->tri[k++] = colour[1];
  draw->tri[k++] = colour[2];
  draw->tri[k++] = x2;
  draw->tri[k++] = y2;
  draw->tri[k++] = 0;
  draw->tri[k++] = 1;
  draw->tri[k++] = colour[0];
  draw->tri[k++] = colour[1];
  draw->tri[k++] = colour[2];
  draw->tri[k++] = x4;
  draw->tri[k++] = y4;
  draw->tri[k++] = 1;
  draw->tri[k++] = 1;
  draw->tri[k++] = colour[0];
  draw->tri[k++] = colour[1];
  draw->tri[k++] = colour[2];
  return k;
}


int draw_enqueue_line(graph_draw *draw, int k, double x1, double y1, double x2, double y2, double w, glm::vec3 colour)
{
  double dx = x2 - x1;
  double dy = y2 - y1;
  double r = sqrt(dx * dx + dy * dy);
  dx /= r;
  dy /= r;
  k = draw_enqueue_quad(draw, k, x1 - w * dy, y1 + w * dx, x2 - w * dy, y2 + w * dx, x1 + w * dy, y1 - w * dx, x2 + w * dy, y2 - w * dx, colour);
  return k;
}

int draw_enqueue_circle(graph_draw *draw, int k, double x, double y, double w, glm::vec3 colour)
{
  k = draw_enqueue_quad(draw, k, x - w, y - w, x + w, y - w, x - w, y + w, x + w, y + w, colour);
  return k;
}

void draw_designer(graph_draw *draw, graph_designer *designer, bool is_active, bool big)
{
  glm::vec3 bg = glm::mix(colours[designer->id], black, 0.75f);
  if (! is_active)
    bg = glm::mix(bg, black, 0.5f);
  glClearColor(bg[0], bg[1], bg[2], 0);
  glClear(GL_COLOR_BUFFER_BIT);
  int k = 0;
  for (int i = 0; i < designer->links.size() && k <= draw->components - 7 * 6; ++i)
  {
    double w = big ? 8 : 16;
    double x1 = designer->links[i]->x1;
    double y1 = designer->links[i]->y1;
    double x2 = designer->links[i]->x2;
    double y2 = designer->links[i]->y2;
    glm::vec3 colour = colours[designer->links[i]->target];
    k = draw_enqueue_line(draw, k, x1, y1, x2, y2, w, colour);
  }
  if (designer->cord)
  {
    double w = big ? 8 : 16;
    double x1 = designer->cord->x1;
    double y1 = designer->cord->y1;
    double x2 = designer->cord->x2;
    double y2 = designer->cord->y2;
    k = draw_enqueue_line(draw, k, x1, y1, x2, y2, w, colours[designer->id]);
  }
  glBufferSubData(GL_ARRAY_BUFFER, 0, k * sizeof(float), &draw->tri[0]);
  glBindVertexArray(draw->vao_quad);
  glUseProgram(draw->prog_line);
  glDrawArrays(GL_TRIANGLES, 0, k / 7);
  k = 0;
  for (int i = 0; i < designer->arrows.size() && k < draw->components - 8 * 3; ++i)
  {
    double x1 = designer->arrows[i]->x1;
    double y1 = designer->arrows[i]->y1;
    double x2 = designer->arrows[i]->x2;
    double y2 = designer->arrows[i]->y2;
    double x3 = designer->arrows[i]->x3;
    double y3 = designer->arrows[i]->y3;
    glm::vec3 colour = colours[designer->arrows[i]->link->target];
    k = draw_enqueue_triangle(draw, k, x1, y1, x2, y2, x3, y3, colour);
  }
  glBufferSubData(GL_ARRAY_BUFFER, 0, k * sizeof(float), &draw->tri[0]);
  glBindVertexArray(draw->vao_tri);
  glUseProgram(draw->prog_arrow);
  glDrawArrays(GL_TRIANGLES, 0, k / 8);
  k = 0;
  for (int i = 0; i < designer->nodes.size() && k <= draw->components - 7 * 6; ++i)
  {
    const double w = 20;
    double x = designer->nodes[i]->x;
    double y = designer->nodes[i]->y;
    glm::vec3 colour = colours[designer->nodes[i]->designer->id];
    if (designer->nodes[i]->fixed)
      colour = white;
    k = draw_enqueue_circle(draw, k, x, y, w, colour);
  }
  glBufferSubData(GL_ARRAY_BUFFER, 0, k * sizeof(float), &draw->tri[0]);
  glBindVertexArray(draw->vao_quad);
  glUseProgram(draw->prog_circle);
  glDrawArrays(GL_TRIANGLES, 0, k / 7);
}

void draw_buttons(graph_draw *draw, glm::vec3 colour, graph_mode mode)
{
  glm::vec3 bg = glm::mix(colour, black, 0.75f);
  glClearColor(bg[0], bg[1], bg[2], 0);
  glClear(GL_COLOR_BUFFER_BIT);
  int k = 0;
  k = draw_enqueue_quad(draw, k, -1, -1, 1, -1, -1, 1, 1, 1, colour);
  glBufferSubData(GL_ARRAY_BUFFER, 0, k * sizeof(float), &draw->tri[0]);
  glBindVertexArray(draw->vao_quad);
  glUseProgram(draw->prog_buttons);
  glUniform1i(draw->u_mode, mode);
  glDrawArrays(GL_TRIANGLES, 0, k / 7);
}

void draw_help(graph_draw *draw, glm::vec3 colour)
{
  glm::vec3 bg = glm::mix(colour, black, 0.75f);
  glClearColor(bg[0], bg[1], bg[2], 0);
  glClear(GL_COLOR_BUFFER_BIT);
  int k = 0;
  k = draw_enqueue_quad(draw, k, -1, -1, 1, -1, -1, 1, 1, 1, colour);
  glBufferSubData(GL_ARRAY_BUFFER, 0, k * sizeof(float), &draw->tri[0]);
  glBindVertexArray(draw->vao_quad);
  glUseProgram(draw->prog_help);
  glDrawArrays(GL_TRIANGLES, 0, k / 7);
}

void debug_program(GLuint program) {
  GLint status = 0;
  glGetProgramiv(program, GL_LINK_STATUS, &status);
  GLint length = 0;
  glGetProgramiv(program, GL_INFO_LOG_LENGTH, &length);
  char *info = 0;
  if (length) {
    info = (char *)malloc(length + 1);
    info[0] = 0;
    glGetProgramInfoLog(program, length, 0, info);
    info[length] = 0;
  }
  if ((info && info[0]) || ! status) {
    fprintf(stderr, "program link info:\n%s", info ? info : "(no info log)");
  }
  if (info) {
    free(info);
  }
}

void debug_shader(GLuint shader, GLenum type, const char *source) {
  GLint status = 0;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
  GLint length = 0;
  glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &length);
  char *info = 0;
  if (length) {
    info = (char *)malloc(length + 1);
    info[0] = 0;
    glGetShaderInfoLog(shader, length, 0, info);
    info[length] = 0;
  }
  if ((info && info[0]) || ! status) {
    const char *type_str = "unknown";
    switch (type) {
      case GL_VERTEX_SHADER: type_str = "vertex"; break;
      case GL_FRAGMENT_SHADER: type_str = "fragment"; break;
      case GL_COMPUTE_SHADER: type_str = "compute"; break;
    }
    fprintf(stderr, "%s shader compile info:\n%s\nshader source:\n%s", type_str, info ? info : "(no info log)", source ? source : "(no source)");
  }
  if (info) {
    free(info);
  }
}

GLuint vertex_fragment_shader(const char *vert, const char *frag) {
  GLuint program = glCreateProgram();
  {
    GLuint shader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(shader, 1, &vert, 0);
    glCompileShader(shader);
    debug_shader(shader, GL_VERTEX_SHADER, vert);
    glAttachShader(program, shader);
    glDeleteShader(shader);
  }
  {
    GLuint shader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(shader, 1, &frag, 0);
    glCompileShader(shader);
    debug_shader(shader, GL_FRAGMENT_SHADER, frag);
    glAttachShader(program, shader);
    glDeleteShader(shader);
  }
  glLinkProgram(program);
  debug_program(program);
  return program;
}

const char *draw_circle_vert =
"#version 330 core\n"
"layout (location = 0) in vec2 pos;\n"
"layout (location = 1) in vec2 tcd;\n"
"layout (location = 2) in vec3 clr;\n"
"out vec2 v_texcoord;\n"
"out vec3 v_colour;\n"
"void main() {\n"
"  gl_Position = vec4(2.0/800.0 * pos - vec2(1.0,1.0), 0.0, 1.0);\n"
"  v_texcoord = tcd * 2.0 - vec2(1.0, 1.0);\n"
"  v_colour = clr;\n"
"}\n"
;

const char *draw_circle_frag =
"#version 330 core\n"
"in vec2 v_texcoord;\n"
"in vec3 v_colour;\n"
"out vec4 colour;\n"
"void main() {\n"
"  float r = length(v_texcoord);\n"
"  float d = mix(length(dFdx(v_texcoord)), length(dFdy(v_texcoord)), 0.5);\n"
"  colour = vec4(v_colour * (smoothstep(0.2, 0.2 + d, r) - smoothstep(0.8 - d, 0.8, r)), 1.0 - smoothstep(1.0 - d, 1.0, r));\n"
"}\n"
;

const char *draw_arrow_vert =
"#version 330 core\n"
"layout (location = 0) in vec2 pos;\n"
"layout (location = 1) in vec3 tcd;\n"
"layout (location = 2) in vec3 clr;\n"
"out vec3 v_texcoord;\n"
"out vec3 v_colour;\n"
"void main() {\n"
"  gl_Position = vec4(2.0/800.0 * pos - vec2(1.0,1.0), 0.0, 1.0);\n"
"  v_texcoord = tcd;\n"
"  v_colour = clr;\n"
"}\n"
;

const char *draw_arrow_frag =
"#version 330 core\n"
"in vec3 v_texcoord;\n"
"in vec3 v_colour;\n"
"out vec4 colour;\n"
"void main() {\n"
"  vec3 w = fwidth(v_texcoord);\n"
"  vec3 a = smoothstep(vec3(0.1), vec3(0.1) + w, v_texcoord);\n"
"  vec3 b = smoothstep(vec3(0.0), vec3(0.0) + w, v_texcoord);\n"
"  float e1 = max(min(min(a.x, a.y), a.z), 1.0 - smoothstep(0.05, 0.05 + length(w), abs(v_texcoord.y - v_texcoord.z)));\n"
"  float e2 = min(min(b.x, b.y), b.z);\n"
"  colour = vec4(v_colour * e1, e2);\n"
"}\n"
;

const char *draw_line_vert =
"#version 330 core\n"
"layout (location = 0) in vec2 pos;\n"
"layout (location = 1) in vec2 tcd;\n"
"layout (location = 2) in vec3 clr;\n"
"out vec2 v_texcoord;\n"
"out vec3 v_colour;\n"
"void main() {\n"
"  gl_Position = vec4(2.0/800.0 * pos - vec2(1.0,1.0), 0.0, 1.0);\n"
"  v_texcoord = tcd * 2.0 - vec2(1.0, 1.0);\n"
"  v_colour = clr;\n"
"}\n"
;

const char *draw_line_frag =
"#version 330 core\n"
"in vec2 v_texcoord;\n"
"in vec3 v_colour;\n"
"out vec4 colour;\n"
"void main() {\n"
"  float r = abs(v_texcoord.x);\n"
"  float d = abs(dFdx(v_texcoord.x)) + abs(dFdy(v_texcoord.x));\n"
"  colour = vec4(v_colour * (1.0 - smoothstep(0.333 - d, 0.333, r)), 1.0 - smoothstep(1.0 - d, 1.0, r));\n"
"}\n"
;

const char *draw_buttons_vert =
"#version 330 core\n"
"layout (location = 0) in vec2 pos;\n"
"layout (location = 1) in vec2 tcd;\n"
"layout (location = 2) in vec3 clr;\n"
"out vec2 v_texcoord;\n"
"out vec3 v_colour;\n"
"void main() {\n"
"  gl_Position = vec4(pos, 0.0, 1.0);\n"
"  v_texcoord = vec2(tcd.y, 1.0 - tcd.x);\n"
"  v_colour = clr;\n"
"}\n"
;

const char *draw_buttons_frag =
"#version 330 core\n"
"uniform sampler2D selected;\n"
"uniform sampler2D deselected;\n"
"uniform int mode;\n"
"in vec2 v_texcoord;\n"
"in vec3 v_colour;\n"
"out vec4 colour;\n"
"void main() {\n"
"  int me = int(floor(v_texcoord.y * 8.0));\n"
"  float a = me == mode ? texture(selected, v_texcoord).r : texture(deselected, v_texcoord).r;\n"
"  colour = vec4(v_colour, a);\n"
"}\n"
;

const char *draw_help_vert =
"#version 330 core\n"
"layout (location = 0) in vec2 pos;\n"
"layout (location = 1) in vec2 tcd;\n"
"layout (location = 2) in vec3 clr;\n"
"out vec2 v_texcoord;\n"
"out vec3 v_colour;\n"
"void main() {\n"
"  gl_Position = vec4(pos, 0.0, 1.0);\n"
"  v_texcoord = vec2(tcd.y, 1.0 - tcd.x);\n"
"  v_colour = clr;\n"
"}\n"
;

const char *draw_help_frag =
"#version 330 core\n"
"uniform sampler2D help;\n"
"in vec2 v_texcoord;\n"
"in vec3 v_colour;\n"
"out vec4 colour;\n"
"void main() {\n"
"  float a = texture(help, v_texcoord).r;\n"
"  colour = vec4(v_colour, a);\n"
"}\n"
;

unsigned char *read_file(const char *name) {
  FILE *f = fopen(name, "rb");
  fseek(f, 0, SEEK_END);
  long len = ftell(f);
  fseek(f, 0, SEEK_SET);
  unsigned char *p = (unsigned char *) malloc(len);
  fread(p, len, 1, f);
  fclose(f);
  return p;
}

graph_draw *draw_new()
{
  graph_draw *draw = new graph_draw();
  draw->components = 4096;
  glGenBuffers(1, &draw->vbo_tri);
  glBindBuffer(GL_ARRAY_BUFFER, draw->vbo_tri);
  glBufferData(GL_ARRAY_BUFFER, draw->components * sizeof(GLfloat), 0, GL_DYNAMIC_DRAW);
  glGenVertexArrays(1, &draw->vao_tri);
  glBindVertexArray(draw->vao_tri);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), 0);
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), ((char *)0) + 2 * sizeof(GLfloat));
  glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), ((char *)0) + 5 * sizeof(GLfloat));
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glEnableVertexAttribArray(2);
  glGenVertexArrays(1, &draw->vao_quad);
  glBindVertexArray(draw->vao_quad);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 7 * sizeof(GLfloat), 0);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 7 * sizeof(GLfloat), ((char *)0) + 2 * sizeof(GLfloat));
  glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, 7 * sizeof(GLfloat), ((char *)0) + 4 * sizeof(GLfloat));
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glEnableVertexAttribArray(2);
  glGenTextures(3, &draw->tex[0]);
  unsigned char *g;
  glActiveTexture(GL_TEXTURE0 + 1);
  glBindTexture(GL_TEXTURE_2D, draw->tex[0]);
  g = read_file("deselected.raw");
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, 240, 800, 0, GL_RED, GL_UNSIGNED_BYTE, g);
  free(g);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glActiveTexture(GL_TEXTURE0 + 2);
  glBindTexture(GL_TEXTURE_2D, draw->tex[1]);
  g = read_file("selected.raw");
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, 240, 800, 0, GL_RED, GL_UNSIGNED_BYTE, g);
  free(g);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glActiveTexture(GL_TEXTURE0 + 0);
  glBindTexture(GL_TEXTURE_2D, draw->tex[2]);
  g = read_file("help.raw");
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, 800, 800, 0, GL_RED, GL_UNSIGNED_BYTE, g);
  free(g);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  draw->prog_circle  = vertex_fragment_shader(draw_circle_vert,  draw_circle_frag);
  draw->prog_arrow   = vertex_fragment_shader(draw_arrow_vert,   draw_arrow_frag);
  draw->prog_line    = vertex_fragment_shader(draw_line_vert,    draw_line_frag);
  draw->prog_buttons = vertex_fragment_shader(draw_buttons_vert, draw_buttons_frag);
  draw->prog_help    = vertex_fragment_shader(draw_help_vert,    draw_help_frag);
  glUseProgram(draw->prog_buttons);
  glUniform1i(glGetUniformLocation(draw->prog_buttons, "deselected"), 1);
  glUniform1i(glGetUniformLocation(draw->prog_buttons, "selected"), 2);
  draw->u_mode = glGetUniformLocation(draw->prog_buttons, "mode");
  glUseProgram(0);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_SCISSOR_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  return draw;
}

struct {
  graph_mode current_mode;
  int active;
  graph_designer *designer[4];
  graph_draw *draw;
  double x, y;
} S;

void keycb(GLFWwindow *window, int key, int scancode, int action, int mods)
{
  glfwSetWindowShouldClose(window, GL_TRUE);
  (void) key;
  (void) scancode;
  (void) action;
  (void) mods;
}

void motioncb(GLFWwindow *window, double x, double y)
{
  y = 800 - 1 - y;
  designer_mouse_move(S.designer[S.active], x - 240, y);
  S.x = x;
  S.y = y;
  (void) window;
}

void buttoncb(GLFWwindow *window, int button, int action, int mods)
{
  if (button == GLFW_MOUSE_BUTTON_LEFT)
  {
    if (S.x < 240)
    {
      int active = 4 - S.y / 200;
      if (active < 0)
        active = 0;
      if (active > 3)
        active = 3;
      designer_leave_mode(S.designer[S.active]);
      S.active = active;
      designer_enter_mode(S.designer[S.active], S.current_mode);
    }
    else if (S.x > 1040)
    {
      int mode = 8 - S.y / 100;
      if (mode < 0)
        mode = 0;
      if (mode > 7)
        mode = 7;
      designer_leave_mode(S.designer[S.active]);
      designer_enter_mode(S.designer[S.active], S.current_mode = graph_mode(mode));
    }
    else
    {
      double x = S.x - 240;
      if (action == GLFW_PRESS)
        designer_mouse_down(S.designer[S.active], x, S.y);
      if (action == GLFW_RELEASE)
        designer_mouse_up(S.designer[S.active], x, S.y);
    }
  }
  (void) window;
  (void) mods;
}

int main()
{
  glfwInit();
  glfwWindowHint(GLFW_CLIENT_API, GLFW_OPENGL_API);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
  GLFWwindow *window = glfwCreateWindow(1280, 800, "graphgrow", 0, 0);
  glfwMakeContextCurrent(window);
  glewExperimental = GL_TRUE;
  glewInit();
  glGetError(); // discard common error from glew 
  glfwSetKeyCallback(window, keycb);
  glfwSetCursorPosCallback(window, motioncb);
  glfwSetMouseButtonCallback(window, buttoncb);

  S.designer[0] = designer_new(0);
  S.designer[1] = designer_new(1);
  S.designer[2] = designer_new(2);
  S.designer[3] = designer_new(3);
  designer_enter_mode(S.designer[S.active = 0], S.current_mode = mode_help);
  S.draw = draw_new();

  lo_address at = lo_address_new("255.255.255.255", "6060");
  lo_address vt = lo_address_new("255.255.255.255", "6061");

  int frame = 0;
  while (1)
  {
    glfwPollEvents();
    if (glfwWindowShouldClose(window)) { break; }

    glScissor(0, 0, 240, 200);
    glViewport(20, 0, 200, 200);
    draw_designer(S.draw, S.designer[3], 3 == S.active, false);

    glScissor(0, 200, 240, 200);
    glViewport(20, 200, 200, 200);
    draw_designer(S.draw, S.designer[2], 2 == S.active, false);

    glScissor(0, 400, 240, 200);
    glViewport(20, 400, 200, 200);
    draw_designer(S.draw, S.designer[1], 1 == S.active, false);

    glScissor(0, 600, 240, 200);
    glViewport(20, 600, 200, 200);
    draw_designer(S.draw, S.designer[0], 0 == S.active, false);

    glScissor(240, 0, 800, 800);
    glViewport(240, 0, 800, 800);
    draw_designer(S.draw, S.designer[S.active], true, true);
    if (S.current_mode == mode_help)
      draw_help(S.draw, colours[S.active]);

    glScissor(1040, 0, 240, 800);
    glViewport(1040, 0, 240, 800);
    draw_buttons(S.draw, colours[S.active], S.current_mode);

    glfwSwapBuffers(window);

    GLint e = glGetError();
    if (e)
      fprintf(stderr, "OpenGL ERROR %d\n", e);

    audio_control a;
    memset(&a, 0, sizeof(a));
    for (int i = 0; i < 4; ++i)
      for (int k = 0; k < 8 && k < S.designer[i]->links.size(); ++k)
      {
        graph_link *l = S.designer[i]->links[k];
        int j = l->target;
        double dx = l->x2 - l->x1;
        double dy = l->y2 - l->y1;
        double ox = l->x2 + l->x1;
        a.scale[i][j][k] = sqrt(dx * dx + dy * dy) / 600;
        a.pan  [i][j][k] = (0.5 * ox - 400) / 400 * 0.5 + 0.5;
        a.level[i][j][k] = 1;
      }
    lo_blob ablob = lo_blob_new(sizeof(a), &a);
    if (ablob)
    {
      if (lo_send(at, "/audio", "b", ablob) == -1)
        fprintf(stderr, "OSC error %d: %s\n", lo_address_errno(at), lo_address_errstr(at));
      lo_blob_free(ablob);
    }
    else
      fprintf(stderr, "OSC error: couldn't lo_blob_new\n");

    video_control v;
    memset(&v, 0, sizeof(v));
    v.active = S.active;
    for (int i = 0; i < 4; ++i)
    {
      v.count[i] = S.designer[i]->links.size();
      for (int j = 0; j < S.designer[i]->links.size() && j < 8; ++j)
      {
        graph_link *l = S.designer[i]->links[j];
        double dx = l->x2 - l->x1;
        double dy = l->y2 - l->y1;
        if (l->flipped)
        {
          dx = -dx;
          dy = -dy;
        }
        double ox = (l->x2 + l->x1) / 2;
        double oy = (l->y2 + l->y1) / 2;
        double s  = sqrt(dx * dx + dy * dy);
        dx /= 600;
        dy /= 600;
        s  /= 600;
        ox -= 400;
        oy -= 400;
        ox /= 600;
        oy /= 600;
        oy = -oy;
        v.source[i][j] = l->target;
        v.scale [i][j] = s;
        glm::mat3 transform = glm::mat3(float(dx), float(dy), float(ox), float(-dy), float(dx), float(oy), 0.0f, 0.0f, 1.0f);
        transform = glm::inverse(transform);
        for (int p = 0; p < 3; ++p)
          for (int q = 0; q < 3; ++q)
            v.transform[i][j][p][q] = transform[p][q];
      }
    }
    lo_blob vblob = lo_blob_new(sizeof(v), &v);
    if (vblob)
    {
      if (lo_send(vt, "/video", "b", vblob) == -1)
        fprintf(stderr, "OSC error %d: %s\n", lo_address_errno(vt), lo_address_errstr(vt));
      lo_blob_free(vblob);
    }
    else
      fprintf(stderr, "OSC error: couldn't lo_blob_new\n");

    frame++;
  }

  glfwDestroyWindow(window);
  glfwTerminate();
  return 0;
}
