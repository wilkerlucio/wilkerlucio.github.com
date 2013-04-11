---
layout: post
title: "RSpec tip: how to control before filters from inner contexts"
description: ""
category: 
tags: []
---
{% include JB/setup %}
I was writing some view specs here and I got into a tricky situation, that is,
for all of the specs I need to run the render method to render a partial view,
this view is the same for all the specs, so, makes total sense to do it into
a before hook, like so:

```ruby
describe "rendering some partial" do
  let(:person) { mock("person") }

  before do
    render partial: "some/partial", locals: {person: person}
  end

  it "renders user name" do
    expect(rendered).to include(person.name)
  end

  it "renders user email" do
    expect(rendered).to include(person.email)
  end
end
```

Fine at this point, but later on I got into a problem, let's say I wanna stub
a method into my person object for some reason.

By looking at my code, you can see that by having the render in a before filter
is a problem on this case, because I need to stub the object _before_ render,
but for that I would need to remove the render from my before hook and manually
call it on each spec, not good...

After some thinking/experimenting I got in a solution that I actually find nice.
The idea is to use `let` statements to controle the running of the before hook.
It's actually easier to show the code:

```ruby
describe "rendering some partial" do
  let(:person) { mock("person") }
  let(:auto_render?) { true }

  def render_view
    render partial: "some/partial", locals: {person: person}
  end

  before do
    render_view if auto_render?
  end

  it "renders user name" do
    expect(rendered).to include(person.name)
  end

  it "renders user email" do
    expect(rendered).to include(person.email)
  end

  context "with a blacklisted email" do
    let(:auto_render?) { false } # RSpec will override inner let statements before run the hooks

    it "shows an alert box" do
      person.stub(blacklisted?: true)
      render_view # now I need to call for render manually
      expect(rendered).to include("Your email is blacklisted!")
    end
  end
end
```

By using `let` values inside of the hooks, I'm able to customize the hooks from
inner contexts :D

If you guys have other ideas for controlling outer hooks from inner contexts I
would love to hear :)

Thanks
