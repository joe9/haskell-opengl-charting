function handle(source, status)
	if (status.kind == "connected") then
		target_displayhint(source, VRESW, VRESH);
		resize_image(source, VRESW, VRESH);
		show_image(source);
		listen();
	elseif (status.kind == "terminated") then
		delete_image(source);
	end
end

function listen()
	target_alloc("demo", handle);
end

function demo()
	listen();
end