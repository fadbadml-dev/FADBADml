#include "badiff.h"
#include "common.cpp"
#include "brusselator.cpp"
#include <iostream>
#include <sys/time.h>
using namespace std;
using namespace fadbad;

struct bad_values {
  double t;
  double x;
  double y;
  double dxdx0;
  double dydx0;
  double dxdy0;
  double dydy0;
};

void print_bad_values(const string name, const bad_values values) {
  cout << name << " = {" << endl;
  print_double("t", values.t);
  cout << endl;
  print_double("x", values.x);
  cout << endl;
  print_double("y", values.y);
  cout << endl;
  print_double("dx/dx0", values.dxdx0);
  cout << endl;
  print_double("dy/dx0", values.dydx0);
  cout << endl;
  print_double("dx/dy0", values.dxdy0);
  cout << endl;
  print_double("dy/dy0", values.dydy0);
  cout << endl << "}";
}

void print_bad_res(result<bad_values> res) {
  print_res(*print_bad_values, res);
}

void main_bad(int nsteps, double dt, result<bad_values> &res) {
  B<double> x, y;
  x = 1; y = 1;
  myvec< B<double> > v(x, y);

  double t = 0;
  timeval tv_start, tv_end;

  gettimeofday(&tv_start, NULL);
  for (int i = 0; i < nsteps; i++) {
    euler(v, dt);
    t += dt;
  }
  gettimeofday(&tv_end, NULL);
  v.x.diff(0,2); v.y.diff(1,2);

  double elapsed_time =
    (tv_end.tv_sec + tv_end.tv_usec / 1000000.0) -
    (tv_start.tv_sec + tv_start.tv_usec / 1000000.0);

  bad_values values;
  values.t = t;
  values.x = v.x.x();
  values.y = v.y.x();
  values.dxdx0 = x.d(0);
  values.dydx0 = x.d(1);
  values.dxdy0 = y.d(0);
  values.dydy0 = y.d(1);

  res.exec_time = elapsed_time ;
  res.dt = dt;
  res.nsteps = nsteps;
  res.values = values;
}

int main() {
  result<bad_values> res;
  main_bad(nsteps, dt, res);
  print_bad_res(res);
  cout << endl;
  return 0;
}
