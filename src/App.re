open Dsl;

let inputToMemberList = (i: string): list(member) => {
  Js_string.split("\n", i)
  |> Array.to_list
  |> List.filter(f => f != "")
  |> List.map(elem => {
       let splitByComma = Js_string.split(",", elem) |> Array.to_list;
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
  let (mms: list(member), setMemberList) = React.useState(() => []);
  let (pastSpeakers: list(member), _) = React.useState(() => []);
  let (currentSpeaker: option(member), _) = React.useState(() => None);

  let onMemberInputChange = value => {
    setMemberList(_ => inputToMemberList(value));
  };

  <>
    <main className="App__main">
      <aside className="App__aside">
        <MemberInput onMemberInputChange />
      </aside>
      <section className="App__sections">
        <AppBar />
        <CurrentSpeaker currentSpeaker />
        <PastSpeakers pastSpeakers=mms />
      </section>
    </main>
  </>;
};