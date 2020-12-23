#include <bits/stdc++.h>
using namespace std;

#define rep(it,st,en) for(ll it=(st);it<(ll)(en);++it)
#define allof(c) (c).begin(), (c).end()
#define allof(c) (c).begin(), (c).end()
#define mt make_tuple
#define mp make_pair
#define pb push_back
#define X first
#define Y second
using ll = int64_t;
using ld = long double;
using vi = vector<ll>;
using vvi = vector<vi>;
using vs = vector<string>;
using pii = pair<ll, ll>;
const ll INF=(ll)1e9; 
const double EPS=(ld)1e-7;

const bool IGNORE_EMPTY_LINES = true;

vs split(const string& str, char c) {
  string segment;
  istringstream in(str);
  vs pieces;
  while (getline(in, segment, c)) {
    pieces.pb(segment);
  }
  return pieces;
}

int main() {
  ifstream in("input");
  string source;
  getline(in, source);
  vs nums = split(source, ',');
  vi intcode;
  for (const string& num : nums) {
    intcode.pb(stoll(num));
  }

  try {
    for (ll pc = 0; pc < intcode.size();) {
      switch (intcode[pc]) {
        case 99:
          throw 0;
        case 1: {
          ll a = intcode[intcode[pc+1]];
          ll b = intcode[intcode[pc+2]];
          ll res = intcode[pc+3];
          intcode[res] = a + b;
          pc += 4;
          break;
        }
        case 2: {
          ll a = intcode[intcode[pc+1]];
          ll b = intcode[intcode[pc+2]];
          ll res = intcode[pc+3];
          intcode[res] = a * b;
          pc += 4;
          break; 
        }
      }
    }
  } catch (...) {
  }

  cout << intcode[0] << endl;
}

