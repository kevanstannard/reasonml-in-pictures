module Maybe = {
  type t('a) =
    | Nothing
    | Just('a);

  let fmap = (f, m) => {
    switch (m) {
    | Nothing => Nothing
    | Just(a) => Just(f(a))
    };
  };

  let (<$>) = fmap;

  let apply = (mf, mv) => {
    switch (mv) {
    | Nothing => Nothing
    | Just(v) =>
      switch (mf) {
      | Nothing => Nothing
      | Just(f) => Just(f(v))
      }
    };
  };

  let (<*>) = apply;

  let bind = (mv, f) => {
    switch (mv) {
    | Nothing => Nothing
    | Just(v) => f(v)
    };
  };

  let (>>=) = bind;
};

Maybe.fmap((+)(3), Just(2));

module Post = {
  type t = {
    id: int,
    title: string,
  };
  let make = (id, title) => {id, title};
  let fmap = (f, post) => f(post);
  let (<$>) = fmap;
  let getPostTitle = post => post.title;
  let findPost = id => make(id, "Post #" ++ string_of_int(id));
};

Post.(fmap(getPostTitle, findPost(1)));
Post.(getPostTitle <$> findPost(1));

module Function = {
  type t('a, 'b) = 'a => 'b;
  let fmap = (f, g, x) => f(g(x));
};

let foo = Function.fmap((+)(3), (+)(2));
foo(10);

Maybe.(Just((+)(3)) <*> Just(2)) == Just(5);

module MyList = {
  type apply('a, 'b) = (list('a => 'b), list('a)) => list('b);
  let apply: apply('a, 'b) =
    (fs, xs) => List.flatten(List.map(f => List.map(f, xs), fs));
  let (<*>) = apply;
};

MyList.([( * )(2), (+)(3)] <*> [1, 2, 3]);

Maybe.((+) <$> Just(5));

/*
 Maybe.(Just((+)(5)) <$> Just(5));
 // Error
 */

Maybe.(Just((+)(5)) <*> Just(5));

let x = Maybe.fmap(( * ), Just(5));
let y = Maybe.apply(x, Just(3));

Maybe.(apply(fmap(( * ), Just(5)), Just(3)));
Maybe.(( * ) <$> Just(5) <*> Just(3));

/*
 See Mutually recursive functions
 https://ocaml.org/learn/tutorials/labels.html
 */
let rec even = x =>
  if (x <= 0) {
    true;
  } else {
    odd(x - 1);
  }
and odd = x =>
  if (x <= 0) {
    false;
  } else {
    even(x - 1);
  };

let half = x =>
  if (even(x)) {
    Maybe.Just(x / 2);
  } else {
    Nothing;
  };

Maybe.(Just(3) >>= half);

Maybe.(Just(20) >>= half >>= half >>= half);

module IO = {
  type t = Js.Promise.t(string);

  exception Error(string);

  type bind('a, 'b) = (t, string => t) => t;
  let bind: bind('a, 'b) = (pa, f) => pa |> Js.Promise.then_(a => f(a));

  let (>>=) = bind;

  type getLine = unit => t;
  let getLine: getLine =
    () =>
      Js.Promise.make((~resolve, ~reject as _reject) => {
        Readline.readline(line => {
          Readline.close();
          resolve(. line);
        })
      });

  type readFile = string => t;
  let readFile: readFile =
    path =>
      Js.Promise.make((~resolve, ~reject) => {
        let onRead =
          (. error, value) => {
            let errorOpt = Js.Nullable.toOption(error);
            switch (errorOpt) {
            | Some(_error) => reject(. Error("Error reading file"))
            | None =>
              let valueOpt = Js.Nullable.toOption(value);
              switch (valueOpt) {
              | None => resolve(. "")
              | Some(value) => resolve(. value)
              };
            };
          };
        Fs.readFile(path, "utf-8", onRead);
      });

  type putStrLn = string => t;
  let putStrLn: putStrLn =
    value =>
      Js.Promise.make((~resolve, ~reject as _) => {
        print_string(value);
        resolve(. value);
      });
};

IO.(getLine() >>= readFile >>= putStrLn);