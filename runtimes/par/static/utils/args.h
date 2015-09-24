#ifndef __UTILS__OPTIONS__H__
#define __UTILS__OPTIONS__H__

#include<string>
#include<map>
#include<stdexcept>
#include "static/utils/ptr.h"


#define EXPECT_ARGUMENT 0
#define EXPECT_VALUE 1
namespace utils {

class Options {
  private:
    std::map<std::string, std::string> opts;
    std::vector<std::string> pos;
  public:
    bool has(const std::string& name) {
      return opts.find(name) != opts.end();
    }
    void set(const std::string& name, const std::string& val) {
      opts[name] = val;
    }
    const std::string& str(int  i) {
      return pos.at(i);
    }
    const std::string& str(const std::string& name) {
      if (!this->has(name)) {
        throw std::invalid_argument("Unknown option '" + name + "'");
      }
      return opts.at(name);
    }
    int integer(int i) {
      return atoi(str(i).c_str());
    }

    int integer(const std::string& i) {
      return atoi(str(i).c_str());
    }

    void parse(int argc, char* argv[]) {
      int state = EXPECT_ARGUMENT;
      std::string key;
      for ( int i = 1; i < argc; i++ ) {
        std::string val(argv[i]);
        switch (state) {
          case EXPECT_ARGUMENT:
            if (val.substr(0, 2) == "--") {
              key = val.substr(2);
              if (key == "")
                throw std::invalid_argument("Option without value");
              state = EXPECT_VALUE;
            } else {
              pos.push_back(val);
            }
            break;
          case EXPECT_VALUE:
            opts[key] = val;
            state = EXPECT_ARGUMENT;
        }
      }
    }
};

}

#endif
