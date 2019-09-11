colorscheme tomorrow-night

hook global InsertChar k %{ try %{
      exec -draft hH <a-k>jk<ret> d 
      exec <esc>
}}

# source "%val{config}/plugins/plug.kak/rc/plug.kak"
# plug "ul/kak-lsp" do %{
#         cargo install --locked --force --path .
# }
#
eval %sh{kak-lsp --kakoune -s $kak_session}
hook global WinSetOption filetype=(rust) %{
        lsp-enable-window
}
