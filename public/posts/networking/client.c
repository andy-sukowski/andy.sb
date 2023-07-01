#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sendfile.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#define PORT "3490"
#define MAXDATASIZE 100

/* get sockaddr, IPv4 or IPv6 */
void *get_in_addr(struct sockaddr *sa)
{
	if (sa->sa_family == AF_INET)
		return &(((struct sockaddr_in *)sa)->sin_addr);

	return &(((struct sockaddr_in6 *)sa)->sin6_addr);
}

/* setup connection for communication */
int tcp_setup(char *hostname)
{
	int sockfd;

	struct addrinfo hints;
	memset(&hints, 0, sizeof hints);
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;

	int rv;
	struct addrinfo *servinfo;
	if ((rv = getaddrinfo(hostname, PORT, &hints, &servinfo)) != 0) {
		fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rv));
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

		if (connect(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
			close(sockfd);
			perror("connect");
			continue;
		}

		break;
	}

	if (!p) {
		fputs("Error: failed to connect\n", stderr);
		return -1;
	}

	char addrstr[INET6_ADDRSTRLEN];
	const char *returnp =
		inet_ntop(p->ai_family,
			  get_in_addr((struct sockaddr *)p->ai_addr), addrstr,
			  sizeof addrstr);
	printf("Connected to %s\n", addrstr);

	freeaddrinfo(servinfo);
	return sockfd;
}

/* read content of file */
int open_img(char *filename, off_t *fsize)
{
	int imgfd = open(filename, O_RDONLY);
	if (imgfd == -1) {
		fprintf(stderr, "Error: failed to open file '%s'\n", filename);
		return -1;
	}

	*fsize = lseek(imgfd, 0, SEEK_END);
	lseek(imgfd, 0, SEEK_SET);

	return imgfd;
}

/* send header to server */
int send_header(int sockfd, char type_code[4], unsigned short width,
		unsigned short height, off_t fsize)
{
	unsigned char header[10];

	/* byte 1 to 4: file type code */
	memcpy(header, type_code, 4);

	/* byte 5 and 6: image width */
	header[4] = (width >> 8) & 0xFF;
	header[5] = width & 0xFF;

	/* byte 7 and 8: image height */
	header[6] = (height >> 8) & 0xFF;
	header[7] = height & 0xFF;

	/* byte 9 and 10: datasize */
	header[8] = (fsize >> 8) & 0xFF;
	header[9] = fsize & 0xFF;

	if (send(sockfd, header, 10, 0) == -1) {
		perror("send");
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

/* send image to server */
int send_img(int sockfd, int imgfd, off_t fsize)
{
	off_t offset = 0, i = 0;
	while (i < fsize) {
		int sent = sendfile(sockfd, imgfd, &offset, BUFSIZ);
		if (sent == -1) {
			perror("send");
			return EXIT_FAILURE;
		}
		i += sent;
	}
	return EXIT_SUCCESS;
}

int main(int argc, char *argv[])
{
	if (argc != 3) {
		fprintf(stderr, "Usage: %s <hostname> <png_in>\n", argv[0]);
		return EXIT_FAILURE;
	}

	int sockfd = tcp_setup(argv[1]);
	if (sockfd == -1)
		return EXIT_FAILURE;

	off_t fsize;
	int imgfd = open_img(argv[2], &fsize);
	if (imgfd == -1) {
		close(sockfd);
		return EXIT_FAILURE;
	}

	char type_code[4] = { 'R', 'G', 'B', '8' };
	int width = 1920, height = 1080;
	if (send_header(sockfd, type_code, width, height, fsize) == EXIT_FAILURE) {
		close(sockfd);
		close(imgfd);
		return EXIT_FAILURE;
	}
	printf("%s %hu %hu %d\n", type_code, width, height, fsize);

	if (send_img(sockfd, imgfd, fsize) == EXIT_FAILURE) {
		close(sockfd);
		close(imgfd);
		return EXIT_FAILURE;
	}

	close(sockfd);
	close(imgfd);
	return EXIT_SUCCESS;
}
