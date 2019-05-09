let str = ReasonReact.string;
let getEventValue = (e): string => ReactEvent.Form.target(e)##value;
let clamp = (lower, upper, value) => {
  switch (value) {
  | _ when value >= upper => upper
  | _ when value <= lower => lower
  | _ => value
  };
};