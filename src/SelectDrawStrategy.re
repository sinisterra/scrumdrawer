open Dsl;
open Utils;

let toValue = (drawStrategy: drawStrategy) => {
  switch (drawStrategy) {
  | ByTeam => "By Team"
  | Random => "Random"
  };
};

[@react.component]
let make = (~drawStrategy, ~onDrawStrategyChange) => {
  let onChange = e => {
    let value = getEventValue(e);

    let selectedOption =
      switch (value) {
      | "By Team" => Some(ByTeam)
      | "Random" => Some(Random)
      | _ => None
      };

    if (Belt.Option.isSome(selectedOption)) {
      onDrawStrategyChange(
        Belt.Option.getWithDefault(selectedOption, ByTeam),
      );
    };

    ();
  };

  <div className="bx--form-item">
    <div className="bx--select">
      <div className="bx--select-input__wrapper">
        <label htmlFor="select-draw-strategy" className="bx--label">
          {"Draw strategy selection" |> str}
        </label>
        <select
          id="select-draw-strategy"
          value={drawStrategy |> toValue}
          onChange
          className="bx--select-input">
          {[ByTeam, Random]
           |> List.map(toValue)
           |> List.map(v =>
                <option
                  value=v
                  className="bx--select-option"
                  selected={v == toValue(drawStrategy)}>
                  {v |> str}
                </option>
              )
           |> Array.of_list
           |> ReasonReact.array}
        </select>
      </div>
    </div>
  </div>;
};