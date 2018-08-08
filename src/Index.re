open Reprocessing;

let width = 600;
let height = 600;
let stepSize = 20;
let gridWidth = width / stepSize;
let gridHeight = height / stepSize;

type posT = (int, int);
let ( *~ ) = ((x1, y1), c) => (x1 * c, y1 * c);
let (+~) = ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2);
let (%~) = ((x1, y1), c) => (x1 mod c, y1 mod c);

type directionT =
  | N
  | S
  | W
  | E;
let dirVector = x =>
  switch (x) {
  | N => (0, (-1))
  | S => (0, 1)
  | W => ((-1), 0)
  | E => (1, 0)
  };

type gameState = {
  snake: list(posT),
  direction: directionT,
  speed: float,
  time: float,
  lastTick: float,
  apple: posT,
};

let rec spawnApple = state => {
  let p = (Random.int(gridWidth), Random.int(gridHeight));
  {...state, apple: p};
};

let setup = env => {
  Env.size(~width, ~height, env);
  {
    snake: [(0, 0), (0, 1), (1, 1)],
    direction: E,
    speed: 5.0,
    time: 0.0,
    lastTick: 0.0,
    apple: (0, 0),
  }
  |> spawnApple;
};

let move = state => {
  let [tail, ...body] = List.rev(state.snake);
  let head = List.hd(state.snake);
  let (x, y) = head +~ dirVector(state.direction);
  {
    ...state,
    snake: [
      (
        (x + width / stepSize) mod (width / stepSize),
        (y + height / stepSize) mod (height / stepSize),
      ),
      ...List.rev(body),
    ],
  }
  |> (
    if (List.hd(state.snake) == state.apple) {
      s => {
        ...spawnApple(s),
        snake: List.append(s.snake, [tail]),
        speed: s.speed +. 1.0,
      };
    } else {
      x => x;
    }
  );
};

let draw = (state, env) => {
  let state =
    if (state.time -. state.lastTick >= 1.0 /. state.speed) {
      {...move(state), lastTick: state.time};
    } else {
      state;
    };
  Draw.background(Constants.black, env);
  Draw.fill(Constants.red, env);
  Draw.rect(
    ~pos=state.apple *~ stepSize,
    ~width=stepSize,
    ~height=stepSize,
    env,
  );
  Draw.fill(Constants.green, env);
  state.snake
  |> List.iter(p =>
       Draw.rect(~pos=p *~ stepSize, ~width=stepSize, ~height=stepSize, env)
     );

  /* let s = setup(env); */
  {...state, time: state.time +. Env.deltaTime(env)};
};

let handleKey = (state, env) => {
  let newDir =
    switch (Env.keyCode(env)) {
    | Right when state.direction != W => E
    | Left when state.direction != E => W
    | Up when state.direction != S => N
    | Down when state.direction != N => S
    | _ => state.direction
    };
  {...state, direction: newDir};
};

run(~setup, ~draw, ~keyTyped=handleKey, ());