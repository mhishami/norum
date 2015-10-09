{application, norum,
 [
  {description, ""},
  {vsn, "0.0.1"},
  {registered, []},
  {modules, ['home_handler','norum_app','norum_sup']},
  {applications, [
                  kernel,
                  stdlib,
                  cowboy,
                  cowlib,
                  ranch,
                  erlydtl
                 ]},
  {mod, { norum_app, []}},
  {env, []}
 ]}.
