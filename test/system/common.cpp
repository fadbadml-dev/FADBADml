#include <iostream>
#include <vector>
using namespace std;

int nsteps = 10;
double dt = 0.001;

template<typename T>
struct result {
  double exec_time;
  double dt;
  int nsteps;
  T values;
};

void print_double(const string name, double f) { cout << name << " = " << f; }
void print_int(const string name, int i) { cout << name << " = " << i; }

void print_double_vector(const string name, vector<double> a) {
  if (a.size() == 0)
    cout << name << " = []";
  else {
    cout << name << " = [" << a[0];
    for(int i = 1; i < a.size(); i++) {
      cout << "; " << a[i];
    }
    cout << "]";
  }
}

template<typename T>
void print_res(void(*print_values)(const string,T), result<T> res) {
  cout << "{" << endl;
  print_double("exec_time", res.exec_time);
  cout << endl;
  print_double("dt", res.dt);
  cout << endl;
  print_int("nsteps", res.nsteps);
  cout << endl;
  print_values("values", res.values);
  cout << endl << "}";
}
