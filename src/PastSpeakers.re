open Utils;
open Dsl;

[@react.component]
let make = (~pastSpeakers: list(member)) => {
  <section className="PastSpeakers">
    <h2> {"List of past speakers" |> str} </h2>
    <ol className="bx--list--ordered">
      {(List.length(pastSpeakers) == 0 ? "no current speakers" : "") |> str}
      {pastSpeakers
       |> List.map(({name, team}) => {
            let teamName = Belt.Option.getWithDefault(team, "No team");

            <li className="bx--list__item PastSpeakers__list__item">
              {name |> str}
              {teamName |> str}
            </li>;
          })
       |> Array.of_list
       |> ReasonReact.array}
    </ol>
  </section>;
};