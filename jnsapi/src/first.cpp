#include <ctime>
#include <iostream>
#include <string>
#include <boost/array.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include "boost/asio.hpp"

using boost::asio::ip::udp;
using namespace std;
using namespace boost;

class udp_channel {

public:

	udp::socket socket_;
	udp_channel *pair_;
	udp::endpoint remote_endpoint_;
	boost::array<char, 1500> recv_buffer_;

	udp_channel(boost::asio::io_service& io_service, udp::endpoint r) :
		socket_(io_service, udp::endpoint(udp::v4(), 0)) {
		remote_endpoint_ = r;
	}

	udp::endpoint get_local_endpoint() {
		return socket_.local_endpoint();
	}

	void set_remote_endpoint(udp::endpoint r) {
		remote_endpoint_ = r;
	}

	void set_pair(udp_channel *c) {
		pair_ = c;
	}

	void start_receive() {
		socket_.async_receive_from(boost::asio::buffer(recv_buffer_),
				remote_endpoint_, boost::bind(&udp_channel::handle_receive,
						this, boost::asio::placeholders::error,
						boost::asio::placeholders::bytes_transferred));
	}

	void handle_receive(const boost::system::error_code& error, std::size_t /*bytes_transferred*/) {
		if (!error || error == boost::asio::error::message_size) {
			boost::shared_ptr<std::string> message(new std::string(
					recv_buffer_.elems));

			pair_->socket_.async_send_to(boost::asio::buffer(*message),
					pair_->remote_endpoint_, boost::bind(
							&udp_channel::handle_send, this, message,
							boost::asio::placeholders::error,
							boost::asio::placeholders::bytes_transferred));

			start_receive();
		}
	}

	void handle_send(boost::shared_ptr<std::string> /*message*/,
			const boost::system::error_code& /*error*/, std::size_t /*bytes_transferred*/) {
	}

};

class relay_channel {
private:
	udp_channel *channel_a_;
	udp_channel *channel_b_;

public:

	relay_channel(boost::asio::io_service& io_service, string host_a,
			int port_a, string host_b, int port_b) {

		try {

			channel_a_ = new udp_channel(io_service,udp::endpoint(asio::ip::address::from_string(host_b), port_b));
			channel_b_ = new udp_channel(io_service,udp::endpoint(asio::ip::address::from_string(host_a), port_a));

			channel_a_->set_pair(channel_b_);
			channel_b_->set_pair(channel_a_);

			channel_a_->start_receive();
			channel_b_->start_receive();

			std::cout << "Local endpoint A: "
					<< channel_a_->get_local_endpoint().port() << std::endl;
			std::cout << "Local endpoint B: "
					<< channel_b_->get_local_endpoint().port() << std::endl;

		} catch (std::exception& e) {
			std::cerr << e.what() << std::endl;
		}

	}

};

int main() {
	try {
		boost::asio::io_service io_service;
		relay_channel r(io_service, "127.0.0.1", 4054, "127.0.0.1", 5045);
		io_service.run();
	} catch (std::exception& e) {
		cout << e.what() << std::endl;
	}

	return 0;
}
