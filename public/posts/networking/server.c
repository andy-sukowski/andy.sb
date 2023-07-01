#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define PORT "3490"
#define BACKLOG 10

/* get sockaddr, IPv4 or IPv6 */
void *get_in_addr(struct sockaddr *sa)
{
	if (sa->sa_family == AF_INET)
		return &(((struct sockaddr_in *)sa)->sin_addr);

	return &(((struct sockaddr_in6 *)sa)->sin6_addr);
}

/* setup connection for communication */
int tcp_setup(void)
{
	int sockfd;

	struct addrinfo hints;
	memset(&hints, 0, sizeof hints);
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE; /* use my IP */

	int rv;
	struct addrinfo *servinfo;
	if ((rv = getaddrinfo(NULL, PORT, &hints, &servinfo)) != 0) {
		fprintf(stderr, "getaddrinfo %s\n", gai_strerror(rv));
		return -1;
	}

	/* loop through results and try to connect */
	struct addrinfo *p;
	for (p = servinfo; p; p = p->ai_next) {
		sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
		if (sockfd == -1) {
			perror("socket");
			continue;
		}

		int yes = 1;
		if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes,
			       sizeof(int)) == -1) {
			close(sockfd);
			perror("setsockopt");
			return -1;
		}

		if (bind(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
			close(sockfd);
			perror("bind");
			continue;
		}

		break;
	}

	freeaddrinfo(servinfo);

	if (!p) {
		fputs("Error: failed to bind\n", stderr);
		return -1;
	}

	if (listen(sockfd, BACKLOG) == -1) {
		perror("listen");
		return -1;
	}

	fputs("Waiting for connections...\n", stdout);
	return sockfd;
}

/* receive header from client */
int recv_header(int sockfd, unsigned char type_code[4], unsigned short *width,
		unsigned short *height, size_t *fsize)
{
	unsigned char header[10];
	if (recv(sockfd, header, 10, 0) == -1) {
		perror("recv");
		return EXIT_FAILURE;
	}

	/* byte 1 to 4: file type code */
	memcpy(type_code, header, 4);

	/* byte 5 and 6: image width */
	*width = (header[4] << 8) | header[5];

	/* byte 7 and 8: image height */
	*height = (header[6] << 8) | header[7];

	/* byte 9 and 10: datasize */
	*fsize = (header[8] << 8) | header[9];

	return EXIT_SUCCESS;
}

/* receive image from client and save */
int recv_save_img(int sockfd, char *filename, size_t fsize)
{
	FILE *fp = fopen(filename, "w");
	if (!fp) {
		fprintf(stderr, "Error: failed to open file '%s'\n", filename);
		return EXIT_FAILURE;
	}

	off_t i = 0;
	while (i < fsize) {
		char buffer[BUFSIZ];
		int received = recv(sockfd, buffer, BUFSIZ, 0);
		if (received == -1) {
			perror("recv");
			return EXIT_FAILURE;
		}
		fwrite(buffer, 1, received, fp);
		i += received;
	}

	fclose(fp);
	return EXIT_SUCCESS;
}

/* handle command-line arguments */
int main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <png_out>\n", argv[0]);
		return EXIT_FAILURE;
	}

	int sockfd = tcp_setup();
	if (sockfd == -1)
		return EXIT_FAILURE;

	struct sockaddr_storage client_addr;
	socklen_t sin_size = sizeof client_addr;
	int newfd = accept(sockfd, (struct sockaddr *)&client_addr, &sin_size);
	if (newfd == -1) {
		perror("accept");
		close(sockfd);
		return EXIT_FAILURE;
	}

	char addrstr[INET6_ADDRSTRLEN];
	inet_ntop(client_addr.ss_family,
		  get_in_addr((struct sockaddr *)&client_addr), addrstr,
		  sizeof addrstr);
	printf("Connected to %s\n", addrstr);

	unsigned char type_code[5];
	unsigned short width, height;
	size_t fsize;
	if (recv_header(newfd, type_code, &width, &height, &fsize) ==
	    EXIT_FAILURE) {
		close(newfd);
		close(sockfd);
		return EXIT_FAILURE;
	}

	type_code[4] = '\0';
	printf("%s %hu %hu %ld\n", type_code, width, height, fsize);

	if (recv_save_img(newfd, argv[1], fsize) == EXIT_FAILURE) {
		close(newfd);
		close(sockfd);
		return EXIT_FAILURE;
	}

	close(newfd);
	close(sockfd);
	return EXIT_SUCCESS;
}
