open Utils;
open Dsl;

[@react.component]
let make = (~pastSpeakers: list(participation)) => {
  <section className="PastSpeakers">
    <h2> {"List of past speakers" |> str} </h2>
    <ol className="bx--list--ordered">
      {(List.length(pastSpeakers) == 0 ? "no current speakers" : "") |> str}
      {pastSpeakers
       |> List.mapi((i, p: participation) => {
            let {member, status} = p;
            let {name, team} = member;
            let teamName = Belt.Option.getWithDefault(team, "No team");

            let participationClass =
              switch (status) {
              | Present => "PastSpeakers__list__item--present"
              | Skipped => "PastSpeakers__list__item--skipped"
              };

            <li
              key={string_of_int(i)}
              className={
                "bx--list__item PastSpeakers__list__item "
                ++ participationClass
              }>
              {name |> str}
            </li>;
          })
       |> Array.of_list
       |> ReasonReact.array}
    </ol>
  </section>;
};