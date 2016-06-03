// The MIT License (MIT)
// 
// Copyright (c) 2013 Joakim Karlsson
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

#ifndef CHOICES_145313413_H
#define CHOICES_145313413_H

namespace choices {

  typedef std::map<std::string, std::string> options;

  inline std::ostream& operator<<(std::ostream& stm, const options& opt)
  {
    options::const_iterator it;
    stm << "{ ";
    for(it = opt.begin(); it != opt.end(); it++)
    {
      stm << (*it).first << ": " << (*it).second << " ";
    }

    stm << "}" << std::endl;

    return stm;
  }

  namespace details {

    inline void as_vect(int argc, const char *argv[], std::vector<std::string>& res)
    {
      res.assign(argv, argv + argc);
    }

    inline std::string remove_prefix(const std::string s)
    {
      size_t pos = s.find("--");
      return pos == std::string::npos ? s : s.substr(pos + 2);
    }

    inline bool is_assignment(const std::string& s)
    {
      return s.find("=") != std::string::npos;
    }

    inline bool is_flag(const std::string& s)
    {
      return s.find("--") == 0 && !is_assignment(s);
    }

    inline std::string flag(const std::string& assignment)
    {
      std::string s = remove_prefix(assignment);
      return s.substr(0, s.find("="));
    }

    inline std::string value(const std::string& assignment)
    {
      return assignment.substr(assignment.find("=")+1);
    }

    inline void get_options(const std::vector<std::string> v, options& opt)
    {
      std::vector<std::string>::const_iterator it;
      for(it = v.begin(); it != v.end(); it++)
      {
        if(is_flag(*it))
        {
          opt[remove_prefix(*it)] = "";
        }
        else if(is_assignment(*it)) 
        {
          opt[flag(*it)] = value(*it);
        }
      }
    }
  }

  namespace d = details;

  inline options parse_cmd(int argc, const char *argv[])
  {
    options o;
    if(argc == 0 || argv == 0)
    {
      return o;
    }

    std::vector<std::string> v;
    d::as_vect(argc, argv, v);

    v.erase(v.begin());

    d::get_options(v, o);

    return o;
  }

  inline bool has_option(const std::string& option, const options& opt)
  {
    return opt.find(option) != opt.end();
  }

  inline const std::string& option_value(const std::string& option, const options& opt)
  {
    options::const_iterator it = opt.find(option);
    return (*it).second;
  }
}

#endif
