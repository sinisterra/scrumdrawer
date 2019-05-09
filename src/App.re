open Utils;
open Dsl;

let inputToMemberList = (i: string): list(member) => {
  Js_string.split("\n", i)
  |> Array.to_list
  |> List.filter(f => f != "")
  |> List.map(elem => {
       let splitByComma = Js_string.split(",", elem) |> Array.to_list;
       Js.log(splitByComma);

       switch (splitByComma) {
       | [name, team] when team != "" => Some({name, team: Some(team)})
       | [name] => Some({name, team: None})
       | [name, team] when team == "" => Some({name, team: None})
       | _ => None
       };
     })
  |> List.fold_left(
       (acc, v) =>
         switch (v) {
         | Some(m) => acc @ [m]
         | None => acc
         },
       [],
     );
};

[@react.component]
let make = () => {
  let (memberList: list(member), setMemberList) = React.useState(() => []);

  let onMemberInputChange = value => {
    Js.log(value);
    let s = inputToMemberList(value);
    setMemberList(_ => s);
    Js.log(s);
  };

  <>
    <main className="App__main">
      <aside className="App__aside">
        <MemberInput onMemberInputChange />
      </aside>
      <section className="App__sections"> {"Panel principal" |> str} </section>
    </main>
  </>;
};