defmodule ExToErl.MixProject do
  use Mix.Project

  def project do
    [
      app: :ex_to_erl,
      version: "0.1.0",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp package() do
    [
      # This option is only needed when you don't want to use the OTP application name
      name: "ex_to_erl",
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/tmbb/ex_to_erl"},
      description: "Convert Elixir expressions into Erlang"
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.21.1", only: [:dev]}
    ]
  end
end
