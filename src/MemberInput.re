open Utils;

[@react.component]
let make = (~onMemberInputChange) => {
  let (memberInput, setMemberInput) = React.useState(() => "Toucan\nDemo");

  React.useEffect1(
    () => {
      onMemberInputChange(memberInput);
      None;
    },
    [|memberInput|],
  );

  let onChange = e => {
    let v = getEventValue(e);
    setMemberInput(_ => v);
  };

  <div>
    <textarea
      value=memberInput
      placeholder="Enter the members of your team, one per line.\nUse a comma to assign the members to a team."
      className="bx--text-area MemberInput__textarea"
      onChange
    />
  </div>;
};