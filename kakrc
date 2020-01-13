colorscheme tomorrow-night

hook global InsertChar k %{ try %{
      exec -draft hH <a-k>jk<ret> d 
      exec <esc>
}}

plug "ul/kak-lsp" do %{
        cargo install --locked --force --path .
}
