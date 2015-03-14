	function clear_screen
	implicit none
	integer*4 lib$erase_page
	integer*4 clear_screen
	clear_screen = lib$erase_page(%ref(1),%ref(1))
	return
	end
