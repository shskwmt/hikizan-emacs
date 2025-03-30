;;; init.el --- -*- lexical-binding: t; -*-
;;(setq debug-on-error t)

(defvar hikizan-dir (file-name-directory load-file-name))
(defvar hikizan-lisp-dir (expand-file-name "lisp" hikizan-dir))

(add-to-list 'load-path hikizan-lisp-dir)

(require 'hikizan-package-manager)
(require 'hikizan-util)
(require 'hikizan-completion)
(require 'hikizan-project)
(require 'hikizan-editor)
(require 'hikizan-org)
(require 'hikizan-snippet)
(require 'hikizan-programming)
(require 'hikizan-llm)
(require 'hikizan-ui)
(require 'hikizan-keybinds)

(setq system-time-local "C") ;; to avoid Japanese in the time stamp

;;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fa2cabf05f4685c111afdc1046886d5ceb94ce0da32050d60cc1e0fb263029fc" "67c2507aa0ebc36889098debdc10756e2689e46c651c58c0311f3a22bfa8e5d3" "141154606abef09690e41a47da20e6351c3c7607a333ae992608b14ad00facef" "e8a44f6247be824353bd7c648a44b065a0fee1408139682aa7b547ce62ea079f" "39c4cdc6f216cac1934b371ca69a40146e40de112e84efda2094dc3d16429682" "1b227f27131d896a74f16025ceb4f7e3506fbe26f7d5ca25807a67cc569aecbc" "14a742adc1735ac16feff040550bbd91b4b43172c82a59ccadaa9cdf524297bd" "59f7daaa31c18319b7dc0aa4f852b376f6ede86ee584dbb12bc3aba78a44dd66" "74cefaf900cacf90462557192d3818d1c346e1d834018832e65cef872b7bea01" "b4a9bb147c17ee41bb90199d0f153cff44c4bd15e1c548c67517ae7bc65696dc" "a01311abdbf2c8305b5c437c1beb0b7f7846a04411ab5b03c05576e1f067ad00" "a99763c0474883b4cf7da44d4ddc01eabdad5fe80d4461985c0e59a5934944fc" "0fbe764a675be7a945af62c3b9a4181d985aa397a5a564badfaada35f282a8c5" "a5e07d6d867c07e5a416091b033782630843ff855a6f285635cbbf13d016ba66" "4b48106838113ec7f119ff4ed99880b7d48bd1ec52f79249c2a9228108fc4f6d" "db18d974cdc4cc9585d2366a9d664d9849a7f78918657b366bede30c58a44f26" "c5b9ab8610d480b1a34ab7bc02a8fc515285083f75cb0de29202880ab63b9252" "bdbdabdd9eb2184b8e25464121194f6ba44f9893f3e9615f5aab97c6dedd2930" "81c774fee1cb54df05aa20acd7c517f86b5942c607e9c5d89153ad7ee8694fe4" "590a3c1232ea516a324356f17780d47425481eed2fa3049f2c83af277633df92" "9c0efad7a8e0ffad7f341303297155650c300d4d8a2f83426fe5851507acb411" "e9c37f3afc5741abe7b1768e201a2854f311b9da84b3a4f1ae6db5e47a0a0cc7" "10147195a09bf14ac2defaa7c7f1f43f964eb40511b97c378cee1439c6db2e99" "9dbd9086e9570b31bb7107fdb9271c3641e55ff0c5db8e8b3b504e796cf39d0a" "c6f7214b4a941285f1fa784583a9a62cea1891ab7822ea52413ed7a66e29e790" "84bc28153ad0f24f4bc5bbfca2a0c1b8b0dc8d405e3517916e6ce06f491050e4" "2acadf9f56a9fc7a2248b1720f7c567eb35c26ded842d11391ec1625cdc1a5b8" "068438c3456ba1361e84e0846853b097bc994f2edb22bb876777069684a61102" "1905f52be14abfbf4332683c6eaa7be83fadd95396089763907ab342f2398839" "0ec3a41fe548cd3b541809e9617edffadcdd4800068d6588c02ecd96b8252045" "6d86b83f6f0934689f61820e1e6d92d29128e622a0c6503580975362ac4fc379" "2afc07d631281c35c64a510d002b8dbb49a9c0c8ad6edc8dc66a0ed14e98b148" "915207fe272dc6f7cb40b10c06a6bf2092742145df18c73fe449573621e04a4f" "426a9b0e9aa78abb0b23b1b69c47f5086cadea443791cd28d8a09b7e7c301e67" "3942a866e2b4a5a22ac374cd7e9cc40984f81b214401ee80189ee717da1952ea" "602d30251f3124c08a4142073f13cb2477c3f99a255269a4b0cf20f812b1cfb4" "beedef2572ea08beb4c83c86b3960c23791e18b3c35dc55fb10aebbc5abac65f" "1e861964006d6310dacdcffc29875f79e70825392b58285a414f6e123b72cec3" "94b5b96f918a0ba998a3bbbc949df98af96a00b208eb00c7b412a9d5cf57cfb5" "8b4200a3dfb6c9af54acd39acc2ea9c23c3c1665eb1bbb4bb6d2fc0cbd7ec06d" "370f0b4ee3da141e74444ff162a71c08fe51090a1c986aabd1950be2766dd0e5" "aaa68cf579a1ff9cb1c42c94eb7fad29574cbd85e2cb16174c73afc68fed03fd" "3d4d41a69a165e0629bc38b42f480ffa46d5462c262e7b2e1540aba7c578a650" "4c385bfc4c13851b71b9c88388a10e9e62d4753f4a2fc76594d59d1ff1bb1577" "dc01a29db7803876c5a327c1b0d445bf9580a8bbf0515b630cc6bdd77aa38e81" "dd8c83478ce607c02945e7fe42e5c534634624a1296591c6e8e310b9a8d6eb38" "9fe340349d9582e1faa9b92d3384a171ffb1f68443e403ac8759a78bd1abe4a7" "bd5345f2005b2093f9792c8e157509383f045ea4c2efd3dedbc4c192e363c60c" "1ada7c5a66b72c1b0b0d2b4b35080a433f9b00fc60adb8758f84028fae48a72a" "046dc446f1229c3192e03084667ef1e755342be4c2aa40c982d2587e8a38fb04" "f934852a61696b806c1a29faf2c3f28ff3f89e748d36e07854dedf3071a7aeb5" "8992dee31510988c7f56a15a839134aa1954ec8a42be5ca8eb6369eb3fd109fc" "de46fb3a2501a4930264f53706cd96db24d3c73e601f537a45bb34f49e3270ae" "c0810c5cd2a6b072e8017e46e5d52cc79a84a094f4b45a6db57260f1fb25838c" "49b84f4468f42609ebc0f33414763cfff3717a4489e4064f7baa859fb6bc0f64" "39877aa5ba4aa3f6374bc199f275a70b6a615a7ee91cbdeed717be94cacf4201" "60edaa7d2b5f20f6a603ce78ea39b17b1cf5763d70dc7098b9dadd791896f4a0" "26d3a734ae1d24df03519068eb72997f790c0a341816de8797d23f34a16db35f" "ca87fe9219804fdb6b90ca17e818c246c1b6f96f43d193073493c2f23dbefdc4" "d4352d0d2466c4c03f1fdedfe30a6e57d1bbb99c9082185ad0a7a4279261fd68" default))
 '(package-selected-packages
   '(avy command-log-mode dockerfile-mode editorconfig ein ellama flycheck git-gutter go-mode lsp-ui magit mozc nerd-icons-completion nerd-icons-corfu nerd-icons-dired orderless popwin rust-mode terraform-mode which-key yaml-mode yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
