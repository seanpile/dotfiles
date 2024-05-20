vim.g.python_host_skip_check = 1
vim.g.python3_host_skip_check = 1
vim.g.python3_host_prog = vim.fn.expand("~/.pyenv/versions/nvim_default/bin/python")

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	{
		"ishan9299/nvim-solarized-lua",
		lazy = false,
		priority = 1000,
		config = function()
			vim.o.background = "dark" -- or 'light'
			vim.cmd.colorscheme("solarized")
		end,
	},
	{ "echasnovski/mini.nvim", version = "*" },
	{ "lewis6991/gitsigns.nvim" },
	{
		"zbirenbaum/copilot.lua",
		event = "InsertEnter",
		config = function()
			require("copilot").setup({
				panel = {
					enabled = false,
				},
				suggestion = {
					debounce = 75,
					auto_trigger = true,
					keymap = {
						accept = "<C-l>",
						next = "<C-j>",
						prev = "<C-k>",
					},
				},
			})
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			local configs = require("nvim-treesitter.configs")

			configs.setup({
				auto_install = true,
				ignore_install = {},
				ensure_installed = { "go", "lua", "vim", "vimdoc", "python", "gitattributes", "gitignore" },
				sync_install = false,
				highlight = { enable = true, disable = { "gitcommit" } },
				indent = { enable = true },
			})
		end,
	},

	{
		"jeetsukumaran/vim-filebeagle",
		keys = {
			{
				"-",
				":FileBeagleBufferDir<CR>",
				noremap = true,
				silent = true,
				desc = "Open file browser in buffers directory.",
			},
			{
				"_",
				":FileBeagle<CR>",
				noremap = true,
				silent = true,
				desc = "Open file browser in current working directory.",
			},
		},
		init = function()
			vim.g.filebeagle_suppress_keymaps = 1
		end,
	},

	------------------------
	-- LSP Support
	------------------------
	{ -- LSP Configuration & Plugins
		"neovim/nvim-lspconfig",
		dependencies = {
			-- Automatically install LSPs and related tools to stdpath for Neovim
			{ "williamboman/mason.nvim", config = true }, -- NOTE: Must be loaded before dependants
			"williamboman/mason-lspconfig.nvim",
			"WhoIsSethDaniel/mason-tool-installer.nvim",

			-- Useful status updates for LSP.
			-- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
			{ "j-hui/fidget.nvim", opts = {} },

			-- `neodev` configures Lua LSP for your Neovim config, runtime and plugins
			-- used for completion, annotations and signatures of Neovim apis
			{ "folke/neodev.nvim", opts = {} },
		},
		config = function()
			-- Brief aside: **What is LSP?**
			--
			-- LSP is an initialism you've probably heard, but might not understand what it is.
			--
			-- LSP stands for Language Server Protocol. It's a protocol that helps editors
			-- and language tooling communicate in a standardized fashion.
			--
			-- In general, you have a "server" which is some tool built to understand a particular
			-- language (such as `gopls`, `lua_ls`, `rust_analyzer`, etc.). These Language Servers
			-- (sometimes called LSP servers, but that's kind of like ATM Machine) are standalone
			-- processes that communicate with some "client" - in this case, Neovim!
			--
			-- LSP provides Neovim with features like:
			--  - Go to definition
			--  - Find references
			--  - Autocompletion
			--  - Symbol Search
			--  - and more!
			--
			-- Thus, Language Servers are external tools that must be installed separately from
			-- Neovim. This is where `mason` and related plugins come into play.
			--
			-- If you're wondering about lsp vs treesitter, you can check out the wonderfully
			-- and elegantly composed help section, `:help lsp-vs-treesitter`

			--  This function gets run when an LSP attaches to a particular buffer.
			--    That is to say, every time a new file is opened that is associated with
			--    an lsp (for example, opening `main.rs` is associated with `rust_analyzer`) this
			--    function will be executed to configure the current buffer
			vim.api.nvim_create_autocmd("LspAttach", {
				group = vim.api.nvim_create_augroup("kickstart-lsp-attach", { clear = true }),
				callback = function(event)
					-- NOTE: Remember that Lua is a real programming language, and as such it is possible
					-- to define small helper and utility functions so you don't have to repeat yourself.
					--
					-- In this case, we create a function that lets us more easily define mappings specific
					-- for LSP related items. It sets the mode, buffer and description for us each time.
					local map = function(keys, func, desc)
						vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
					end

					local pick = function(s)
						return function()
							require("mini.extra").pickers.lsp({ scope = s })
						end
					end

					-- Jump to the definition of the word under your cursor.
					--  This is where a variable was first declared, or where a function is defined, etc.
					--  To jump back, press <C-t>.
					map("gd", vim.lsp.buf.definition, "[G]oto [D]efinition")

					-- Find references for the word under your cursor.
					map("gr", pick("references"), "[G]oto [R]eferences")

					-- Jump to the implementation of the word under your cursor.
					--  Useful when your language has ways of declaring types without an actual implementation.
					map("gI", pick("implementation"), "[G]oto [I]mplementation")

					-- Jump to the type of the word under your cursor.
					--  Useful when you're not sure what type a variable is and you want to see
					--  the definition of its *type*, not where it was *defined*.
					map("<leader>D", pick("type_definition"), "Type [D]efinition")

					-- Fuzzy find all the symbols in your current document.
					--
					map("<leader>ds", pick("document_symbol"), "[D]ocument [S]ymbols")

					-- Fuzzy find all the symbols in your current workspace.
					--  Similar to document symbols, except searches over your entire project.
					--map('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

					-- Rename the variable under your cursor.
					--  Most Language Servers support renaming across files, etc.
					map("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")

					-- Execute a code action, usually your cursor needs to be on top of an error
					-- or a suggestion from your LSP for this to activate.
					map("<leader>co", function()
						vim.lsp.buf.code_action({ apply = true })
					end, "[C]ode actions")

					-- Opens a popup that displays documentation about the word under your cursor
					--  See `:help K` for why this keymap.
					map("K", vim.lsp.buf.hover, "Hover Documentation")

					-- WARN: This is not Goto Definition, this is Goto Declaration.
					--  For example, in C this would take you to the header.
					map("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")

					local client = vim.lsp.get_client_by_id(event.data.client_id)
					if client and client.server_capabilities.documentHighlightProvider then
						local highlight_augroup =
							vim.api.nvim_create_augroup("kickstart-lsp-highlight", { clear = false })

						vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
							buffer = event.buf,
							group = highlight_augroup,
							callback = vim.lsp.buf.document_highlight,
						})

						vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
							buffer = event.buf,
							group = highlight_augroup,
							callback = vim.lsp.buf.clear_references,
						})
					end
				end,
			})

			vim.api.nvim_create_autocmd("LspDetach", {
				group = vim.api.nvim_create_augroup("kickstart-lsp-detach", { clear = true }),
				callback = function(event)
					vim.lsp.buf.clear_references()
					vim.api.nvim_clear_autocmds({ group = "kickstart-lsp-highlight", buffer = event.buf })
				end,
			})

			-- LSP servers and clients are able to communicate to each other what features they support.
			--  By default, Neovim doesn't support everything that is in the LSP specification.
			--  When you add nvim-cmp, luasnip, etc. Neovim now has *more* capabilities.
			--  So, we create new capabilities with nvim cmp, and then broadcast that to the servers.
			local capabilities = vim.lsp.protocol.make_client_capabilities()
			--capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())
			--capabilities.textDocument.completion.completionItem.snippetSupport = false

			-- Enable the following language servers
			--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
			--
			--  Add any additional override configuration in the following tables. Available keys are:
			--  - cmd (table): Override the default command used to start the server
			--  - filetypes (table): Override the default list of associated filetypes for the server
			--  - capabilities (table): Override fields in capabilities. Can be used to disable certain LSP features.
			--  - settings (table): Override the default settings passed when initializing the server.
			--        For example, to see the options for `lua_ls`, you could go to: https://luals.github.io/wiki/settings/
			local servers = {
				jsonls = {},
				gopls = {
					single_file_support = true,
				},
				pyright = {
					autostart = false,
				},
				-- clangd = {},
				-- rust_analyzer = {},
				-- ... etc. See `:help lspconfig-all` for a list of all the pre-configured LSPs
				--
				-- Some languages (like typescript) have entire language plugins that can be useful:
				--    https://github.com/pmizio/typescript-tools.nvim
				--
				-- But for many setups, the LSP (`tsserver`) will work just fine
				-- tsserver = {},
				--

				lua_ls = {
					-- cmd = {...},
					-- filetypes = { ...},
					-- capabilities = {},
					settings = {
						Lua = {
							completion = {
								callSnippet = "Replace",
							},
							-- You can toggle below to ignore Lua_LS's noisy `missing-fields` warnings
							diagnostics = { disable = { "missing-fields" } },
						},
					},
				},
			}

			-- Ensure the servers and tools above are installed
			--  To check the current status of installed tools and/or manually install
			--  other tools, you can run
			--    :Mason
			--
			--  You can press `g?` for help in this menu.
			require("mason").setup()

			-- You can add other tools here that you want Mason to install
			-- for you, so that they are available from within Neovim.
			local ensure_installed = vim.tbl_keys(servers or {})
			vim.list_extend(ensure_installed, {
				"stylua", -- Used to format Lua code
			})
			require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

			require("mason-lspconfig").setup({
				handlers = {
					function(server_name)
						local server = servers[server_name] or {}
						-- This handles overriding only values explicitly passed
						-- by the server configuration above. Useful when disabling
						-- certain features of an LSP (for example, turning off formatting for tsserver)
						server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
						require("lspconfig")[server_name].setup(server)
					end,
				},
			})
		end,
	},

	{ -- Autoformat
		"stevearc/conform.nvim",
		lazy = false,
		keys = {
			{
				"<leader>cf",
				function()
					require("conform").format({ async = true, lsp_fallback = true })
				end,
				mode = "",
				desc = "[F]ormat buffer",
			},
		},
		opts = {
			notify_on_error = false,
			format_on_save = function(bufnr)
				local disable_filetypes = { c = true, cpp = true }
				return {
					timeout_ms = 500,
					lsp_fallback = not disable_filetypes[vim.bo[bufnr].filetype],
				}
			end,
			formatters_by_ft = {
				lua = { "stylua" },
				go = { "goimports", "gofumpt" },
			},
		},
	},
})

-- Basic Keymaps
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Basic Options
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4

vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>", { desc = "Clear search highlights" })
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })
vim.keymap.set("n", "<leader>s", ":wa<CR>", { desc = "Save file" })
vim.keymap.set("n", "<leader>zz", ":e ~/.config/nvim-mini/init.lua<CR>", { desc = "Edit nvim configuration" })

-- Window management
vim.keymap.set("n", "<leader>ww", ":vsp<cr>", { desc = "New vertical split window" })
vim.keymap.set("n", "<leader>wx", ":sp<cr>", { desc = "New horizontal split window" })
vim.keymap.set("n", "<leader>wo", ":only<cr>", { desc = "Close other windows" })
vim.keymap.set("n", "<leader>wp", ":close<cr>", { desc = "Close current window" })

-- Keep cursor centered when scrolling down/up
vim.keymap.set("n", "J", "mzJ`z", { desc = "Join lines and keep cursor on existing location" })
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Scroll down half page and keep cursor centered" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Scroll up half page and keep cursor centered" })
vim.keymap.set("n", "n", "nzzzv", { desc = "Move to next search result and keep cursor centered" })
vim.keymap.set("n", "N", "Nzzzv", { desc = "Move to previous search result and keep cursor centered" })

-- Close vim if last window is the quick fix window
vim.api.nvim_create_autocmd("WinEnter", { command = [[ if winnr('$') == 1 && &buftype == "quickfix"|q|endif ]] })
vim.api.nvim_create_autocmd("FileType", {
	pattern = { "help", "startuptime", "qf", "lspinfo", "diagnostic" },
	command = [[ nnoremap <buffer><silent> q :close<CR>]],
})

require("nvim-treesitter").setup()

------------------------------------------
-- Better basics
------------------------------------------
require("mini.extra").setup()
require("mini.basics").setup({
	options = {
		win_borders = "single",
	},
	mappings = {
		windows = true,
	},
})

------------------------------------------
-- Restore last cursor position
------------------------------------------
require("mini.misc").setup_restore_cursor()

------------------------------------------
-- Better buffer deletion
------------------------------------------
local bufremove = require("mini.bufremove")
bufremove.setup()
vim.keymap.set("n", "<leader><bs>", bufremove.unshow_in_window, { desc = "Remove buffer" })

------------------------------------------
-- Better commenting
------------------------------------------
require("mini.comment").setup()

------------------------------------------
-- Better pairs
------------------------------------------
require("mini.pairs").setup()

------------------------------------------
-- Better statusline
------------------------------------------
require("mini.statusline").setup({})

------------------------------------------
-- Better pickers
------------------------------------------
require("mini.visits").setup()

local pick = require("mini.pick")
pick.setup({
	source = { show = pick.default_show },
	mappings = {
		move_down = "<C-j>",
		move_up = "<C-k>",
	},
	options = {
		content_from_bottom = true,
	},
	window = {
		config = {
			relative = "editor",
			anchor = "SW",
			width = 140,
			height = 20,
		},
	},
})

vim.keymap.set("n", "<leader>/", pick.builtin.grep_live, { desc = "Search in project files" })
vim.keymap.set("n", "<leader>f", pick.builtin.files, { desc = "Open file" })
vim.keymap.set("n", "<leader>b", pick.builtin.buffers, { desc = "Switch buffer" })
vim.keymap.set("n", "<leader>p.", function()
	require("mini.extra").pickers.visit_paths()
end, { desc = "Open recent files" })
vim.keymap.set("n", "<leader><leader>", pick.builtin.resume, { desc = "Resume last picker" })
vim.cmd([[highlight MiniPickMatchCurrent guifg=#268bd2]])

------------------------------------------
-- Better handling of <cr> when popups are open
------------------------------------------
local keys = {
	["cr"] = vim.api.nvim_replace_termcodes("<CR>", true, true, true),
	["ctrl-y"] = vim.api.nvim_replace_termcodes("<C-y>", true, true, true),
	["ctrl-y_cr"] = vim.api.nvim_replace_termcodes("<C-y><CR>", true, true, true),
}

_G.cr_action = function()
	if vim.fn.pumvisible() ~= 0 then
		-- If popup is visible, confirm selected item or add new line otherwise
		local item_selected = vim.fn.complete_info()["selected"] ~= -1
		return item_selected and keys["ctrl-y"] or keys["ctrl-y_cr"]
	else
		-- If popup is not visible, use plain `<CR>`. You might want to customize
		-- according to other plugins. For example, to use 'mini.pairs', replace
		-- next line with `return require('mini.pairs').cr()`
		return keys["cr"]
	end
end

vim.keymap.set("i", "<CR>", "v:lua._G.cr_action()", { expr = true })
